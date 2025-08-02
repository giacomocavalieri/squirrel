import argv
import envoy
import filepath
import gleam/bool
import gleam/erlang/atom
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import shellout
import simplifile
import temporary

/// An integration test for a specific postgres type.
///
type TestCase {
  /// This tests squirrel on a fixed set of parametrised queries doing
  /// insertions and selects. To do so it creates some Gleam code that uses
  /// Gleam values as parameters to the query.
  ///
  TestType(postgres_type: String, values: List(TestValue))

  /// This tests squirrel running a fixed query with no parameters. And it
  /// checks the value you get back from the query to match the given
  /// `expected_value`.
  ///
  TestQuery(query: String)
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
    /// let assert Ok(pog.Returned(_, [result])) sql.select_one(db)
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
  TestType("citext", [TestValue("\"Jak\"")]),
  // Integers
  TestType("int2", [TestValue("1")]),
  TestType("int4", [TestValue("1")]),
  TestType("int8", [TestValue("1")]),
  // Floats
  TestType("float4", [TestValue("1.0")]),
  TestType("float8", [TestValue("1.0")]),
  TestType("numeric", [TestValue("1.0")]),
  TestQuery("select 1::numeric"),
  // Uuid
  TestType("uuid", [TestValue("uuid.v7()")]),
  // Bytea
  TestType("bytea", [TestValue("<<1, 2, 3>>")]),
  // Date
  TestType("date", [TestValue("calendar.Date(1998, calendar.October, 11)")]),
  // TimeOfDay
  TestType("time", [TestValue("calendar.TimeOfDay(1, 11, 10, 0)")]),
  // Timestamp
  TestType("timestamp", [TestValue("timestamp.from_unix_seconds(1000)")]),
  // Array
  TestType("int[]", [TestValue("[1, 2, 3]")]),
  TestType("squirrel_colour[]", [TestValue("[sql.Red, sql.Grey]")]),
  // Custom enums
  TestType(
    "squirrel_colour",
    [TestValue("sql.LightBrown"), TestValue("sql.Red"), TestValue("sql.Grey")],
  ),
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
  #(atom.create("timeout"), seconds, [fun])
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
  values: List(TestCase),
) -> Result(Result(String, #(Int, String)), simplifile.FileError) {
  let integration_test_project = filepath.join(".", "integration_test_project")
  let _ = simplifile.create_directory(integration_test_project)
  use dir <- temporary.create(
    temporary.directory() |> temporary.in_directory(integration_test_project),
  )

  scaffold_gleam_project(dir)
  let code = {
    use code, test_case <- list.fold(values, "")
    let assertions = setup_and_generate_code(for: test_case, in: dir)
    code <> "\n\n" <> assertions
  }
  write_main(code, to: dir)
  test_project(dir)
}

fn setup_and_generate_code(for test_case: TestCase, in dir: String) -> String {
  case test_case {
    TestQuery(query:) -> create_query_files_and_assertions_for_query(query, dir)
    TestType(postgres_type:, values:) -> {
      let table_name = safe_name(postgres_type) <> "_table"
      create_database_table(table_name, postgres_type)
      create_query_files_and_assertions(postgres_type, table_name, values, dir)
    }
  }
}

fn create_database_table(table_name: String, postgres_type: String) -> Nil {
  let drop = "drop table if exists " <> table_name
  let create = "create table if not exists " <> table_name <> "(
      id bigserial primary key,
      col " <> postgres_type <> " not null
    )"

  let assert Ok(database_url) = envoy.get("DATABASE_URL")
  let assert Ok(_) =
    shellout.command(
      run: "psql",
      with: [database_url, "-c" <> drop],
      in: ".",
      opt: [],
    )

  let assert Ok(_) =
    shellout.command(
      run: "psql",
      with: [database_url, "-c" <> create],
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
let assert Ok(pog.Returned(1, [])) = sql.<insert>(db, input)
let assert Ok(pog.Returned(1, [res])) = sql.<select_all>(db)
case expected_output == res.col {
  True -> Nil
  False -> {
    io.println(string.inspect(res.col))
    panic as \" test for <postgres_type> type failed\"
  }
}
let assert Ok(pog.Returned(1, [])) = sql.<delete>(db)
"
    |> string.replace(each: "<postgres_type>", with: postgres_type)
    |> string.replace(each: "<heading>", with: heading)
    |> string.replace(each: "<insert>", with: insert)
    |> string.replace(each: "<select_all>", with: select_all)
    |> string.replace(each: "<delete>", with: delete)
  }

  string.join(assertions, with: "\n")
}

fn create_query_files_and_assertions_for_query(
  query: String,
  dir: String,
) -> String {
  let src_dir = filepath.join(dir, "src")
  let sql_dir = filepath.join(src_dir, "sql")
  let query_name = "query_" <> int.to_string(unique_integer())
  let assert Ok(_) =
    simplifile.write(query, to: filepath.join(sql_dir, query_name <> ".sql"))

  "let assert Ok(_) = sql.<query>(db)"
  |> string.replace(each: "<query>", with: query_name)
}

@external(erlang, "squirrel_ffi", "unique")
fn unique_integer() -> Int

/// Writes the entry point of the Gleam project that will connect to the
/// database and run all the assertions testing the generated squirrel code.
///
fn write_main(assertions: String, to dir: String) -> Nil {
  let main = "
import gleam/erlang/process
import gleam/io
import gleam/json
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import pog
import sql
import youid/uuid

pub fn main() {
  let name = process.new_name(\"test\")
  let config =
    pog.Config(
      ..pog.default_config(name),
      port: 5432,
      user: \"squirrel_test\",
      host: \"localhost\",
      database: \"squirrel_test\",
    )
  let assert Ok(actor) = pog.start(config)
  let db = actor.data

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

fn safe_name(string: String) -> String {
  let assert Ok(regex) = regexp.from_string("[()\\[\\]]")

  let safe_string = regexp.replace(each: regex, with: "_", in: string)
  case string.ends_with(string, "[]") {
    False -> safe_string
    True -> safe_string <> "array"
  }
}
