import envoy
import filepath
import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{Uri}
import gleam_community/ansi
import simplifile
import squirrel/internal/database/postgres
import squirrel/internal/error.{
  type Error, CannotWriteToFile, InvalidConnectionString,
}
import squirrel/internal/project
import squirrel/internal/query.{type TypedQuery}
import term_size

const squirrel_version = "v1.8.1"

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
/// > when connecting, it will read the `DATABASE_URL` env variable that has to
/// > be a valid connection string with the following format:
/// >
/// > ```txt
/// > postgres://user:password@host:port/database
/// > ```
/// >
/// > If a `DATABASE_URL` variable is not set, Squirrel will instead read your
/// > [Postgres env variables](https://www.postgresql.org/docs/current/libpq-envars.html)
/// > and use the following defaults if one is not set:
/// > - `PGHOST`: `"localhost"`
/// > - `PGPORT`: `5432`
/// > - `PGUSER`: `"root"`
/// > - `PGDATABASE`: the name of your Gleam project
/// > - `PGPASSWORD`: `""`
///
/// > ⚠️ The generated code relies on the
/// > [`gleam_pgo`](https://hexdocs.pm/gleam_pgo/) and
/// > [`decode`](https://hexdocs.pm/decode/) packages to work, so make sure to
/// > add those as dependencies to your project.
///
pub fn main() {
  case connection_options() {
    // In case we cannot read the connection options we just immediately fail.
    Error(error) -> {
      error.to_doc(error)
      |> doc.to_string(term_width())
      |> io.println

      exit(1)
    }
    // Otherwise we can walk through the file system and type all the queries,
    // connecting to the database.
    Ok(options) -> {
      let #(report, status_code) =
        walk(project.src())
        |> run(options)
        |> pretty_report

      io.println(report)
      exit(status_code)
    }
  }
}

/// Returns the connection options to use to connect to the database.
/// It first tries to read and parse a `DATABASE_URL` env variable (failing if
/// it has an invalid format).
///
/// If the `DATABASE_URL` variable is not set, it uses the Postgres env vars and
/// some defaults if any of those are not set.
///
fn connection_options() -> Result(postgres.ConnectionOptions, Error) {
  case envoy.get("DATABASE_URL") {
    Ok(url) ->
      parse_connection_url(url)
      |> result.replace_error(InvalidConnectionString(url))
    Error(_) -> Ok(connection_options_from_variables())
  }
}

const default_host = "localhost"

const default_user = "postgres"

const default_database = "database"

const default_password = ""

const default_port = 5432

const default_timeout = 1000

/// Creates a `ConnectionOptions` reading values from env variables and falling
/// back to some defaults if any required one is not set.
///
fn connection_options_from_variables() -> postgres.ConnectionOptions {
  let host = envoy.get("PGHOST") |> result.unwrap(default_host)
  let user = envoy.get("PGUSER") |> result.unwrap(default_user)
  let password = envoy.get("PGPASSWORD") |> result.unwrap(default_password)
  let database =
    envoy.get("PGDATABASE")
    |> result.or(project.name())
    |> result.unwrap(default_database)
  let port =
    envoy.get("PGPORT")
    |> result.then(int.parse)
    |> result.unwrap(default_port)

  postgres.ConnectionOptions(
    host:,
    port:,
    user:,
    password:,
    database:,
    timeout: default_timeout,
  )
}

/// Parses a connection string into a `ConnectionOptions` failing if it has an
/// invalid format instead of silently producing a default one.
///
fn parse_connection_url(raw: String) -> Result(postgres.ConnectionOptions, Nil) {
  use uri <- result.try(uri.parse(raw))
  let Uri(scheme:, userinfo:, host:, port:, path:, ..) = uri
  use _ <- result.try(check_scheme(scheme))
  let #(user, password) = parse_user_and_password_from_userinfo(userinfo)
  let database = parse_database_from_path(path)

  Ok(postgres.ConnectionOptions(
    host: host |> option.unwrap(default_host),
    port: port |> option.unwrap(default_port),
    user: user |> option.unwrap(default_user),
    password: password |> option.unwrap(default_password),
    database: database |> option.unwrap(default_database),
    timeout: default_timeout,
  ))
}

fn check_scheme(scheme: Option(String)) -> Result(Nil, Nil) {
  case scheme {
    Some("postgres") | Some("postgresql") | None -> Ok(Nil)
    Some(_) -> Error(Nil)
  }
}

fn parse_user_and_password_from_userinfo(
  userinfo: Option(String),
) -> #(Option(String), Option(String)) {
  case userinfo {
    None -> #(None, None)
    Some(userinfo) ->
      case string.split(userinfo, on: ":") {
        [user] -> #(Some(user), None)
        [user, password, ..] -> #(Some(user), Some(password))
        _ -> #(None, None)
      }
  }
}

fn parse_database_from_path(path: String) -> Option(String) {
  case string.split(path, "/") {
    ["", database, ..] -> Some(database)
    _ -> None
  }
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
  let code = query.generate_code(queries, squirrel_version)
  let try_write =
    simplifile.write(code, to: file)
    |> result.map_error(CannotWriteToFile(file, _))

  use _ <- result.try(try_write)
  Ok(list.length(queries))
}

// --- PRETTY REPORT PRINTING --------------------------------------------------

fn term_width() -> Int {
  term_size.columns() |> result.unwrap(80)
}

fn pretty_report(dirs: Dict(String, #(Int, List(Error)))) -> #(String, Int) {
  let #(ok, errors) = {
    use #(all_ok, all_errors), _, result <- dict.fold(dirs, #(0, []))
    let #(ok, errors) = result
    #(all_ok + ok, errors |> list.append(all_errors))
  }
  let errors_doc =
    list.map(errors, error.to_doc)
    |> doc.join(with: doc.lines(2))

  let status_code = case errors {
    [_, ..] -> 1
    [] -> 0
  }

  let report = case ok, errors {
    0, [_, ..] -> doc.to_string(errors_doc, term_width())
    0, [] ->
      [
        text_with_header(
          "🐿️  ",
          "I couldn't find any `*.sql` file to generate queries from.",
        ),
        doc.lines(2),
        flexible_string(
          "Hint: I look for all `*.sql` files in any directory called `sql`
under your project's `src` directory.",
        ),
      ]
      |> doc.concat
      |> doc.to_string(term_width())
      |> ansi.yellow

    n, [] ->
      text_with_header(
        "🐿️  ",
        "Generated "
          <> int.to_string(n)
          <> " "
          <> pluralise(n, "query", "queries")
          <> "!",
      )
      |> doc.to_string(term_width())
      |> ansi.green

    n, [_, ..] ->
      [
        errors_doc,
        doc.lines(2),
        text_with_header(
          "🥜 ",
          "I could still generate "
            <> int.to_string(n)
            <> " "
            <> pluralise(n, "query", "queries")
            <> ".",
        ),
      ]
      |> doc.concat
      |> doc.to_string(term_width())
  }

  #(report, status_code)
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

// --- FFI ---------------------------------------------------------------------

@external(erlang, "squirrel_ffi", "exit")
pub fn exit(n: Int) -> Nil
