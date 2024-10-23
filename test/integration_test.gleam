import argv
import filepath
import gleam/bool
import gleam/erlang
import gleam/erlang/atom
import gleam/io
import gleam/list
import gleam/regex
import gleam/string
import shellout
import simplifile
import temporary

/// An integration test for a specific postgres type.
type TestType {
  TestType(postgres_type: String, values: List(TestValue))
}

/// How a tested value looks in gleam code when it is fed into a query or
/// returned as a result.
///
type TestValue {
  TestInputOutput(
    /// How the value is supposed to look like when passed in as a query
    /// argument to a squirrel generated query:
    ///
    /// ```gleam
    /// let assert Ok(_) =
    ///   sql.insert(db, <input>)
    /// //               ^^^^^^^ This will be replaced with the input string
    /// ```
    ///
    input: String,
    /// How the value is supposed to look like when returned as an output
    /// from a squirrel generated query:
    ///
    /// ```gleam
    /// let assert Ok(pgo.Returned(_, [result])) sql.select_one(db)
    /// let assert <output> = result.col
    /// //         ^^^^^^^^ This will be replaced with the output string
    /// ```
    ///
    output: String,
  )
  TestValue(value: String)
}

const integration_tests = [
  // Booleans
  TestType("bool", [TestValue("True"), TestValue("False")]),
  // Text data
  TestType("text", [TestValue("\"hello\"")]),
  TestType("char(1)", [TestValue("\"j\"")]),
  TestType("bpchar", [TestValue("\"j\"")]),
  TestType("varchar(3)", [TestValue("\"jak\"")]),
  // Integers
  TestType("int2", [TestValue("1")]),
  TestType("int4", [TestValue("1")]),
  TestType("int8", [TestValue("1")]),
  // Floats
  TestType("float4", [TestValue("1.0")]),
  TestType("float8", [TestValue("1.0")]),
  TestType("numeric", [TestValue("1.0")]),
  // Uuid
  TestType("uuid", [TestValue("uuid.v7()")]),
  // Bytea
  TestType("bytea", [TestValue("<<1, 2, 3>>")]),
  // Date
  TestType("date", [TestValue("#(1998, 10, 11)")]),
  // Timestamp
  TestType("timestamp", [TestValue("#(#(1998, 10, 11), #(22, 10, 00))")]),
  // Array
  TestType("int[]", [TestValue("[1, 2, 3]")]),
  // Json
  TestType(
    "json",
    [
      TestInputOutput(
        input: "json.object([#(\"a\", json.int(1))])",
        output: "\"{\\\"a\\\":1}\"",
      ),
    ],
  ),
  TestType(
    "jsonb",
    [
      TestInputOutput(
        input: "json.object([#(\"a\", json.int(1))])",
        output: "\"{\\\"a\\\": 1}\"",
      ),
    ],
  ),
]

const project_toml = "name = \"integration_test_project\"
version = \"1.0.0\"

[dependencies]
squirrel = { path = \"../..\" }
"

fn with_timeout(seconds seconds: Int, run fun: fn() -> a) {
  #(atom.create_from_string("timeout"), seconds, [fun])
}

pub fn integration_test_() {
  // This test takes a while since it has to compile an entire gleam project.
  // So we have to increase the timeout
  use <- with_timeout(seconds: 60)

  // This test takes some time se we skip it unless one passes the "integration"
  // flag from command line.
  use <- bool.guard(when: argv.load().arguments != ["integration"], return: Nil)

  let assert Ok(result) = run_integration_tests(integration_tests)
  case result {
    Ok(_) -> Nil
    Error(#(_status_code, message)) -> {
      io.println(message)
      panic as "integration test failed"
    }
  }

  Nil
}

fn run_integration_tests(
  values: List(TestType),
) -> Result(Result(String, #(Int, String)), simplifile.FileError) {
  let integration_test_project = filepath.join(".", "integration_test_project")
  let _ = simplifile.create_directory(integration_test_project)
  use dir <- temporary.create(
    temporary.directory() |> temporary.in_directory(integration_test_project),
  )

  scaffold_gleam_project(dir)
  let code = {
    use code, TestType(postgres_type, values) <- list.fold(values, "")
    let table_name = safe_name(postgres_type) <> "_table"
    create_database_table(table_name, postgres_type)
    let assertions =
      create_query_files_and_assertions(postgres_type, table_name, values, dir)
    code <> "\n\n" <> assertions
  }
  write_main(code, to: dir)

  let _ = erlang.get_line(">")
  test_project(dir)
}

fn create_database_table(table_name: String, postgres_type: String) -> Nil {
  let drop = "drop table if exists " <> table_name
  let create = "create table if not exists " <> table_name <> "(
      id bigserial primary key,
      col " <> postgres_type <> " not null
    )"

  let assert Ok(_) =
    shellout.command(
      run: "psql",
      with: ["-Usquirrel_test", "-dsquirrel_test", "-c" <> drop],
      in: ".",
      opt: [],
    )

  let assert Ok(_) =
    shellout.command(
      run: "psql",
      with: ["-Usquirrel_test", "-dsquirrel_test", "-c" <> create],
      in: ".",
      opt: [],
    )

  Nil
}

/// Creates the most basic structure of a gleam project needed to run the
/// integration tests inside `dir`.
///
fn scaffold_gleam_project(dir: String) -> Nil {
  // We set up the canonical squirrel project structure.
  //
  let src_dir = filepath.join(dir, "src")
  let sql_dir = filepath.join(src_dir, "sql")
  let assert Ok(_) = simplifile.create_directory(src_dir)
  let assert Ok(_) = simplifile.create_directory(sql_dir)

  // Write the `gleam.toml` needed to run the project.
  //
  let assert Ok(_) =
    simplifile.write(project_toml, to: filepath.join(dir, "gleam.toml"))

  Nil
}

/// Following squirrel's project structure, this writes a file for each query to
/// be tested on the given table and returns the Gleam code that runs the
/// assertions to check everything works fine.
///
fn create_query_files_and_assertions(
  postgres_type: String,
  table_name: String,
  values: List(TestValue),
  dir: String,
) -> String {
  let src_dir = filepath.join(dir, "src")
  let sql_dir = filepath.join(src_dir, "sql")

  // Then we create all the queries to be fed into squirrel.
  //
  let insert = table_name <> "_insert"
  let assert Ok(_) =
    "insert into <table>(col) values ($1)"
    |> string.replace(each: "<table>", with: table_name)
    |> simplifile.write(to: filepath.join(sql_dir, insert <> ".sql"))

  let select_all = table_name <> "_select_all"
  let assert Ok(_) =
    "select col from <table>"
    |> string.replace(each: "<table>", with: table_name)
    |> simplifile.write(to: filepath.join(sql_dir, select_all <> ".sql"))

  let delete = table_name <> "_delete_rows"
  let assert Ok(_) =
    "delete from <table>"
    |> string.replace(each: "<table>", with: table_name)
    |> simplifile.write(to: filepath.join(sql_dir, delete <> ".sql"))

  let assertions = {
    use value <- list.map(values)
    let heading = case value {
      TestInputOutput(input:, output:) -> "
        let input = " <> input <> "
        let expected_output = " <> output <> "\n"
      TestValue(value:) -> "
        let input = " <> value <> "
        let expected_output = input"
    }

    "
<heading>
let assert Ok(pgo.Returned(1, [])) = sql.<insert>(db, input)
let assert Ok(pgo.Returned(1, [res])) = sql.<select_all>(db)
case expected_output == res.col {
  True -> Nil
  False -> {
    io.println(string.inspect(res.col))
    panic as \" test for <postgres_type> type failed\"
  }
}
let assert Ok(pgo.Returned(1, [])) = sql.<delete>(db)
"
    |> string.replace(each: "<postgres_type>", with: postgres_type)
    |> string.replace(each: "<heading>", with: heading)
    |> string.replace(each: "<insert>", with: insert)
    |> string.replace(each: "<select_all>", with: select_all)
    |> string.replace(each: "<delete>", with: delete)
  }

  string.join(assertions, with: "\n")
}

/// Writes the entry point of the Gleam project that will connect to the
/// database and run all the assertions testing the generated squirrel code.
///
fn write_main(assertions: String, to dir: String) -> Nil {
  let main = "
import gleam/io
import gleam/string
import gleam/pgo
import gleam/json
import youid/uuid
import sql

pub fn main() {
let config =
  pgo.Config(
    ..pgo.default_config(),
    port: 5432,
    user: \"squirrel_test\",
    host: \"localhost\",
    database: \"squirrel_test\",
  )
let db = pgo.connect(config)

" <> assertions <> "
}"

  let src_dir = filepath.join(dir, "src")
  let assert Ok(_) =
    simplifile.write(main, to: filepath.join(src_dir, "main.gleam"))

  Nil
}

/// First runs the squirrel command to generate code and then the main module of
/// the project to run all the generated assertions.
///
fn test_project(dir: String) -> Result(String, #(Int, String)) {
  let assert Ok(_) =
    shellout.command(
      run: "gleam",
      with: ["run", "-msquirrel"],
      in: dir,
      opt: [],
    )

  shellout.command(run: "gleam", with: ["run", "-mmain"], in: dir, opt: [])
}

fn safe_name(string: String) {
  let assert Ok(regex) = regex.from_string("[()\\[\\]]")
  regex.replace(each: regex, with: "_", in: string)
}
