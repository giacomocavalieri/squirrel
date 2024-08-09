import envoy
import filepath
import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import gleam_community/ansi
import simplifile
import squirrel/internal/database/postgres
import squirrel/internal/error.{type Error, CannotWriteToFile}
import squirrel/internal/query.{type TypedQuery}
import term_size

const squirrel_version = "v1.0.0"

/// 🐿️ Performs code generation for your Gleam project.
///
/// `squirrel` is not configurable and will discover the queries to generate
/// code for by relying on a conventional project's structure:
/// - `squirrel` first looks for all directories called `sql` under the `src`
///   directory of your Gleam project, and reads all the `*.sql` files in there
///   (in glob terms `src/**/sql/*.sql`).
/// - Each `*.sql` file _must contain a single query_ as it is turned into a
///   Gleam function with the same name.
/// - All functions coming from the same `sql` directory will be grouped under
///   a Gleam file called `sql.gleam` at the same level: given a `src/$PATH/sql`
///   directory, you'll end up with a generated `src/$PATH/sql.gleam` file.
///
/// > ⚠️ In order to generate type safe code, `squirrel` has to connect
/// > to your Postgres database. To know what host, user, etc. values to use
/// > when connecting, it will read your
/// > [Postgres env variables.](https://www.postgresql.org/docs/current/libpq-envars.html)
/// >
/// > If a variable is not set it will go with the following defaults:
/// > - `PGHOST`: `"localhost"`
/// > - `PGPORT`: `5432`
/// > - `PGUSER`: `"root"`
/// > - `PGDATABASE`: `"database"`
/// > - `PGPASSWORD`: `""`
///
/// > ⚠️ The generated code relies on the
/// > [`gleam_pgo`](https://hexdocs.pm/gleam_pgo/) and
/// > [`decode`](https://hexdocs.pm/decode/) packages to work, so make sure to
/// > add those as dependencies to your project.
///
pub fn main() {
  walk("src")
  |> run(read_connection_options())
  |> pretty_report
  |> io.println
}

fn read_connection_options() -> postgres.ConnectionOptions {
  let host = envoy.get("PGHOST") |> result.unwrap("localhost")
  let user = envoy.get("PGUSER") |> result.unwrap("root")
  let database = envoy.get("PGDATABASE") |> result.unwrap("database")
  let password = envoy.get("PGPASSWORD") |> result.unwrap("")
  let port =
    envoy.get("PGPORT")
    |> result.then(int.parse)
    |> result.unwrap(5432)

  postgres.ConnectionOptions(
    host: host,
    port: port,
    user: user,
    password: password,
    database: database,
    timeout: 1000,
  )
}

/// Finds all `from/**/sql` directories and lists the full paths of the `*.sql`
/// files inside each one.
///
fn walk(from: String) -> Dict(String, List(String)) {
  case filepath.base_name(from) {
    "sql" -> {
      let assert Ok(files) = simplifile.read_directory(from)
      let files = {
        use file <- list.filter_map(files)
        use extension <- result.try(filepath.extension(file))
        use <- bool.guard(when: extension != "sql", return: Error(Nil))
        let file_name = filepath.join(from, file)
        case simplifile.is_file(file_name) {
          Ok(True) -> Ok(file_name)
          Ok(False) | Error(_) -> Error(Nil)
        }
      }
      dict.from_list([#(from, files)])
    }

    _ -> {
      let assert Ok(files) = simplifile.read_directory(from)
      let directories = {
        use file <- list.filter_map(files)
        let file_name = filepath.join(from, file)
        case simplifile.is_directory(file_name) {
          Ok(True) -> Ok(file_name)
          Ok(False) | Error(_) -> Error(Nil)
        }
      }

      list.map(directories, walk)
      |> list.fold(from: dict.new(), with: dict.merge)
    }
  }
}

/// Given a dict of directories and their `*.sql` files, performs code
/// generation for each one, bundling all `*.sql` files under the same directory
/// into a single Gleam module.
///
fn run(
  directories: Dict(String, List(String)),
  connection: postgres.ConnectionOptions,
) -> Dict(String, #(Int, List(Error))) {
  use directory, files <- dict.map_values(directories)

  let #(queries, errors) =
    list.map(files, query.from_file)
    |> result.partition

  let #(queries, errors) = case postgres.main(queries, connection) {
    Error(error) -> #([], [error, ..errors])
    Ok(#(queries, type_errors)) -> #(queries, list.append(errors, type_errors))
  }

  let output_file =
    filepath.directory_name(directory)
    |> filepath.join("sql.gleam")

  case write_queries(queries, to: output_file) {
    Ok(n) -> #(n, errors)
    Error(error) -> #(list.length(queries), [error, ..errors])
  }
}

fn write_queries(
  queries: List(TypedQuery),
  to file: String,
) -> Result(Int, Error) {
  use <- bool.guard(when: queries == [], return: Ok(0))

  let directory = filepath.directory_name(file)
  let _ = simplifile.create_directory_all(directory)

  // We need the top level imports.
  let #(count, code, imports) = {
    let acc = #(0, "", set.new())
    use #(count, code, imports), query <- list.fold(queries, acc)
    let #(query_code, query_imports) =
      query.generate_code(squirrel_version, query)

    #(
      count + 1,
      code <> "\n" <> query_code,
      imports |> set.union(query_imports),
    )
  }

  let imports =
    set.to_list(imports) |> list.sort(string.compare) |> string.join(with: "\n")
  let code = imports <> "\n" <> code

  let try_write =
    simplifile.write(code, to: file)
    |> result.map_error(CannotWriteToFile(file, _))

  use _ <- result.try(try_write)
  Ok(count)
}

// --- PRETTY REPORT PRINTING --------------------------------------------------

fn pretty_report(dirs: Dict(String, #(Int, List(Error)))) -> String {
  let width = term_size.columns() |> result.unwrap(80)
  let #(ok, errors) = {
    use #(all_ok, all_errors), _, result <- dict.fold(dirs, #(0, []))
    let #(ok, errors) = result
    #(all_ok + ok, errors |> list.append(all_errors))
  }
  let errors_doc =
    list.map(errors, error.to_doc)
    |> doc.join(with: doc.lines(2))

  case ok, errors {
    0, [_, ..] -> doc.to_string(errors_doc, width)
    0, [] ->
      text_with_header(
        "🐿️  ",
        "I couldn't find any `*.sql` file to generate queries from",
      )
      |> doc.to_string(width)
      |> ansi.yellow

    n, [] ->
      text_with_header(
        "🐿️  ",
        "Generated "
          <> int.to_string(n)
          <> " "
          <> pluralise(n, "query", "queries"),
      )
      |> doc.to_string(width)
      |> ansi.green
      |> string.append("\n")
      |> string.append(
        text_with_header(
          "🥜 ",
          "Don't forget to run `gleam add decode gleam_pgo` if you haven't yet!",
        )
        |> doc.to_string(width)
        |> ansi.cyan,
      )

    n, [_, ..] ->
      [
        errors_doc,
        doc.lines(2),
        text_with_header(
          "🥜 ",
          "I could still generate "
            <> int.to_string(n)
            <> " "
            <> pluralise(n, "query", "queries"),
        ),
      ]
      |> doc.concat
      |> doc.to_string(width)
  }
}

fn text_with_header(header: String, text: String) {
  [
    doc.from_string(header),
    flexible_string(text)
      |> doc.nest(by: string.length(header)),
  ]
  |> doc.concat
  |> doc.group
}

fn pluralise(count: Int, singular: String, plural: String) -> String {
  case count {
    1 -> singular
    _ -> plural
  }
}

fn flexible_string(string: String) -> Document {
  string.split(string, on: "\n")
  |> list.flat_map(string.split(_, on: " "))
  |> list.map(doc.from_string)
  |> doc.join(with: doc.flex_space)
  |> doc.group
}
