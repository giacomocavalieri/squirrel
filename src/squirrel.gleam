import argv
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
import glexer
import glexer/token
import simplifile
import squirrel/internal/database/postgres
import squirrel/internal/error.{
  type Error, CannotReadFile, CannotWriteToFile, InvalidConnectionString,
  OutdatedFile,
}
import squirrel/internal/project
import squirrel/internal/query.{type TypedQuery}
import term_size

const squirrel_version = "v2.1.0"

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
/// > [`pog`](https://hexdocs.pm/pog/) and
/// > [`decode`](https://hexdocs.pm/decode/) packages to work, so make sure to
/// > add those as dependencies to your project.
///
pub fn main() {
  case parse_cli_args(), connection_options() {
    Error(Nil), _ -> {
      help_text()
      |> doc.to_string(term_width())
      |> io.println

      exit(1)
    }

    // In case we cannot read the connection options we just immediately fail.
    _, Error(error) -> {
      error.to_doc(error)
      |> doc.to_string(term_width())
      |> io.println

      exit(1)
    }

    // Otherwise we can walk through the file system and type all the queries,
    // connecting to the database.
    Ok(mode), Ok(options) -> {
      let generated_queries =
        walk(project.src())
        |> generate_queries(options)

      let #(report, status_code) = case mode {
        GenerateCode ->
          generated_queries
          |> write_queries
          |> report_written_queries

        CheckGeneratedCode ->
          generated_queries
          |> check_queries
          |> report_checked_queries
      }

      io.println(report)
      exit(status_code)
    }
  }
}

// --- CLI ARGS PARSING --------------------------------------------------------

type Mode {
  GenerateCode
  CheckGeneratedCode
}

fn parse_cli_args() -> Result(Mode, Nil) {
  case argv.load().arguments {
    [] -> Ok(GenerateCode)
    ["check"] -> Ok(CheckGeneratedCode)
    _ -> Error(Nil)
  }
}

fn help_text() -> Document {
  let nesting = 4
  let title_line = doc.from_string("🐿️  Squirrel " <> squirrel_version)

  let usage_line =
    doc.concat([
      doc.zero_width_string(ansi.yellow("Usage: ")),
      doc.zero_width_string(ansi.green("gleam run -m squirrel")),
      doc.from_string(" [COMMAND]"),
    ])

  let check_command =
    [
      doc.from_string("check  "),
      "checks the generated code is up to date with the sql files"
        |> flexible_string
        |> doc.nest(by: 7),
    ]
    |> doc.concat
    |> doc.group

  let commands =
    [
      doc.zero_width_string(ansi.yellow("Commands:")),
      doc.nest(check_command, by: nesting),
    ]
    |> doc.join(with: doc.nest(doc.line, by: nesting))

  [title_line, usage_line, commands]
  |> doc.join(with: doc.lines(2))
}

// --- CONNECTION OPTIONS PARSING ----------------------------------------------

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

// --- MODE: GENERATE QUERIES --------------------------------------------------

/// Given a dict of directories and their `*.sql` files, performs code
/// generation for each one, bundling all `*.sql` files under the same directory
/// into a single Gleam module.
///
fn generate_queries(
  directories: Dict(String, List(String)),
  connection: postgres.ConnectionOptions,
) -> Dict(String, #(List(TypedQuery), List(Error))) {
  use _directory, files <- dict.map_values(directories)

  let #(queries, errors) =
    list.map(files, query.from_file)
    |> result.partition

  case postgres.main(queries, connection) {
    Error(error) -> #([], [error, ..errors])
    Ok(#(queries, type_errors)) -> #(queries, list.append(errors, type_errors))
  }
}

/// Given the queries generated by `generate_queries`, tries to write those to
/// their own file and returns a dictionary that - for each file - holds the
/// number of queries that could be generated and a list of all the errors that
/// took place.
///
fn write_queries(
  queries: Dict(String, #(List(TypedQuery), List(Error))),
) -> Dict(String, #(Int, List(Error))) {
  use directory, #(queries, errors) <- dict.map_values(queries)
  let output_file = directory_to_output_file(directory)
  case write_queries_to_file(queries, to: output_file) {
    Ok(n) -> #(n, errors)
    Error(error) -> #(list.length(queries), [error, ..errors])
  }
}

fn write_queries_to_file(
  queries: List(TypedQuery),
  to file: String,
) -> Result(Int, Error) {
  use <- bool.guard(when: queries == [], return: Ok(0))
  let directory = filepath.directory_name(file)
  let _ = simplifile.create_directory_all(directory)

  let code = query.generate_code(queries, squirrel_version)
  let try_write =
    simplifile.write(code, to: file)
    |> result.map_error(CannotWriteToFile(file, _))

  use _ <- result.try(try_write)
  Ok(list.length(queries))
}

fn directory_to_output_file(directory: String) -> String {
  filepath.directory_name(directory)
  |> filepath.join("sql.gleam")
}

// --- MODE: CHECK GENERATED CODE ----------------------------------------------

fn check_queries(
  queries: Dict(String, #(List(query.TypedQuery), List(Error))),
) -> Dict(String, Result(Nil, List(Error))) {
  use directory, #(queries, errors) <- dict.map_values(queries)
  case errors {
    // If there's any error in the query generation (for example there's a piece
    // of sql code with a syntax error, or a column doesn't exist, ...)
    // we don't even try to check that the files match, but just report the
    // errors that took place.
    [_, ..] -> Error(errors)
    [] -> {
      let output_file = directory_to_output_file(directory)
      case simplifile.read(output_file) {
        Error(reason) -> Error([CannotReadFile(file: output_file, reason:)])
        Ok(actual_code) ->
          case check_queries_code(queries, actual_code) {
            Different -> Error([OutdatedFile(file: output_file)])
            Same -> Ok(Nil)
          }
      }
    }
  }
}

@internal
pub type CheckResult {
  Different
  Same
}

/// Checks that the given `file`'s code is the same that would be generated from
/// the given `queries`.
/// Returns `Same` if the code is ok, `Different` otherwise.
///
/// > ⚠️ Note how this comparison doesn't take comments and whitespace into
/// > account! If the only thing that changed are comments and/or formatting
/// > two files will still be considered the same.
///
fn check_queries_code(
  queries: List(TypedQuery),
  actual_code: String,
) -> CheckResult {
  let expected_code = query.generate_code(queries, squirrel_version)
  compare_code_snippets(actual_code, expected_code)
}

@internal
pub fn compare_code_snippets(
  actual_code: String,
  expected_code: String,
) -> CheckResult {
  let actual_tokens =
    glexer.new(actual_code)
    |> glexer.discard_comments
    |> glexer.discard_whitespace
    |> glexer.lex

  let expected_tokens =
    glexer.new(expected_code)
    |> glexer.discard_comments
    |> glexer.discard_whitespace
    |> glexer.lex

  compare_token_lists(expected_tokens, actual_tokens)
}

fn compare_token_lists(
  expected_tokens: List(#(token.Token, glexer.Position)),
  actual_tokens: List(#(token.Token, glexer.Position)),
) {
  case expected_tokens, actual_tokens {
    [], [] -> Same
    [], _ | _, [] -> Different
    [#(expected_token, _position), ..expected_rest],
      [#(actual_token, _position), ..actual_rest]
    ->
      case expected_token == actual_token {
        True -> compare_token_lists(expected_rest, actual_rest)
        False -> Different
      }
  }
}

// --- PRETTY REPORT PRINTING --------------------------------------------------

fn term_width() -> Int {
  term_size.columns() |> result.unwrap(80)
}

fn report_written_queries(
  dirs: Dict(String, #(Int, List(Error))),
) -> #(String, Int) {
  let #(ok, errors) = {
    use acc, _, #(oks, errors) <- dict.fold(dirs, #(0, []))
    let #(all_ok, all_errors) = acc
    #(all_ok + oks, errors |> list.append(all_errors))
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

fn report_checked_queries(dirs: Dict(String, Result(Nil, List(Error)))) {
  let errors = {
    use all_errors, _, result <- dict.fold(dirs, [])
    case result {
      Error(errors) -> errors |> list.append(all_errors)
      Ok(_) -> all_errors
    }
  }

  let status_code = case errors {
    [_, ..] -> 1
    [] -> 0
  }

  let report = case errors {
    [_, ..] ->
      list.map(errors, error.to_doc)
      |> doc.join(with: doc.lines(2))
      |> doc.to_string(term_width())

    [] ->
      text_with_header("🐿️  ", "All good!")
      |> doc.to_string(term_width())
      |> ansi.green
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
fn exit(n: Int) -> Nil
