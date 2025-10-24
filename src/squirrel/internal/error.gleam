import glam/doc.{type Document}
import gleam/bit_array
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import gleam_community/ansi
import mug
import simplifile

pub type Error {
  // --- POSTGRES RELATED ERRORS -----------------------------------------------
  /// When we can't establish a TCP connection to the server.
  ///
  PgCannotEstablishTcpConnection(
    host: String,
    port: Int,
    reason: mug.ConnectError,
  )

  /// When the server immediately closes the connection right after we try to
  /// connect using the given username and database.
  ///
  PgInvalidUserDatabase(user: String, database: String)

  /// When we receive an unexpected message while waiting for the authentication
  /// methods allowed by the server.
  ///
  PgUnexpectedAuthMethodMessage(expected: String, got: String)

  /// When the password provided to authenticate is invalid.
  ///
  PgInvalidPassword(user: String)

  /// When the cleartext authentication workflow goes wrong.
  ///
  PgUnexpectedCleartextAuthMessage(expected: String, got: String)

  /// When the sals-sha256 authentication workflow goes wrong.
  ///
  PgUnexpectedSha256AuthMessage(expected: String, got: String)

  /// When sasl-sha256 authentication fails because the server sends us the
  /// wrong proof back.
  ///
  PgInvalidSha256ServerProof

  /// When the server is expecting an authentication method that is not
  /// currently supported.
  ///
  PgUnsupportedAuthentication(auth: String)

  /// When there's an error with the underlying socket and I cannot send
  /// messages to the server.
  ///
  PgCannotSendMessage(reason: String)

  /// This comes from the `postgres_protocol` module, it happens if the server
  /// sends back a malformed message or there's a type of messages decoding is
  /// not implemented for.
  /// This should never happen and warrants a bug report!
  ///
  PgCannotDecodeReceivedMessage(reason: String)

  /// When there's an error with the underlying socket and I cannot receive
  /// messages to the server.
  ///
  PgCannotReceiveMessage(reason: String)

  /// When I cannot get a query description back from the postgres server.
  ///
  PgCannotDescribeQuery(
    file: String,
    query_name: String,
    expected: String,
    got: String,
  )

  /// When a query fails because the authenticated user doesn't have the
  /// permission to run it (for example if they can't access a table that
  /// belongs to another user).
  ///
  PgPermissionDenied(query_file: String, reason: String)

  /// When there's an error with the message exhange during the query explaining
  /// phase needed to figure out the nullability of the returned values.
  ///
  PgCannotExplainQuery(
    file: String,
    query_name: String,
    expected: String,
    got: String,
  )

  // --- OTHER GENERIC ERRORS --------------------------------------------------
  /// When the connection string provided as an env variable cannot be parsed.
  ///
  InvalidConnectionString(string: String)

  /// When I cannot read a file containing queries.
  ///
  CannotReadFile(file: String, reason: simplifile.FileError)

  /// When the generated code cannot be written to a file.
  ///
  CannotWriteToFile(file: String, reason: simplifile.FileError)

  /// If an ".sql" file holding a query has a name that is not a valid Gleam
  /// name.
  /// Instead of trying to magically come up with a name we fail and report the
  /// error.
  ///
  QueryFileHasInvalidName(
    file: String,
    suggested_name: Option(String),
    reason: ValueIdentifierError,
  )

  /// If a query returns a column that is not a valid Gleam identifier. Instead
  /// of trying to magically come up with a name we fail and report the error.
  ///
  QueryHasInvalidColumn(
    file: String,
    column_name: String,
    suggested_name: Option(String),
    content: String,
    starting_line: Int,
    reason: ValueIdentifierError,
  )

  /// When there's a param/return type that cannot be converted into a Gleam
  /// type.
  ///
  QueryHasUnsupportedType(
    file: String,
    name: String,
    content: String,
    starting_line: Int,
    type_: String,
  )

  /// When a query param/return type is an enum that cannot be automatically
  /// converted into a Gleam type definition.
  ///
  QueryHasInvalidEnum(
    file: String,
    content: String,
    starting_line: Int,
    enum_name: String,
    reason: EnumError,
  )

  /// If the query contains an error and cannot be parsed by the DBMS.
  ///
  CannotParseQuery(
    file: String,
    name: String,
    content: String,
    starting_line: Int,
    error_code: Option(String),
    pointer: Option(Pointer),
    additional_error_message: Option(String),
    hint: Option(String),
  )

  /// If the query returns multiple values with the same name. This is an error
  /// as the generated Gleam type would have duplicate fields. For example:
  ///
  /// ```sql
  /// select
  ///   1 as wibble,
  ///   2 as wibble
  /// --     ^^^^^^ they have the same name!
  /// ```
  ///
  QueryReturnsMultipleValuesWithTheSameName(
    file: String,
    content: String,
    starting_line: Int,
    names: List(String),
  )

  /// If the postgres server sends in a query explanantion in a format that I
  /// cannot parse.
  ///
  /// This should never happen, and if it does it means I grossly forgot about
  /// a possible value I shouldn't so I have to ask to open an issue.
  ///
  CannotParsePlanForQuery(file: String, reason: json.DecodeError)

  /// If, when trying to run `explain (generic_plan)` the query fails because
  /// that option is not recognised, that means that the Postgres version is
  /// older than 16 and should then be updated to use squirrel.
  ///
  PostgresVersionIsTooOld

  /// This happens when the version number returned by the Postgres server is
  /// in an invalid format (that is it's not UTF8 encoded nor a valid integer
  /// number).
  ///
  PostgresVersionHasInvalidFormat(version: BitArray)

  /// This happens when we're checking the generated code with the `--check`
  /// flag and the generated code is not in sync with the actual sql queries
  /// that would be generated by squirrel.
  ///
  OutdatedFile(file: String)

  /// This happens when the code generated by Squirrel would end up overriding
  /// an existing file that was not generated by Squirrel.
  /// We really want to avoid this, otherwise we'd end up deleting human written
  /// code!
  ///
  /// For example if I have an already existing `my_app/sql.gleam` file, I don't
  /// want Squirrel to silently replace its content with the generated code!
  ///
  CannotOverwriteExistingFile(file: String)
}

pub type ValueIdentifierError {
  ValueContainsInvalidGrapheme(at: Int, grapheme: String)
  ValueIsEmpty
}

pub type TypeIdentifierError {
  TypeContainsInvalidGrapheme(at: Int, grapheme: String)
  TypeIsEmpty
}

pub type EnumError {
  EnumWithNoVariants
  InvalidEnumName(name: String)
  InvalidEnumVariants(fields: List(String))
}

/// Used to literally point to a particular piece of a string and attach a
/// message to that point.
///
pub type Pointer {
  Pointer(point_to: PointerKind, message: String)
}

/// A pointer could either point to a specific byte of a String or it could
/// point at a specific word (in that case it will point to the first occurrence
/// of such word).
///
pub type PointerKind {
  Name(name: String)
  ByteIndex(position: Int)
}

pub fn to_doc(error: Error) -> Document {
  // Errors as they are, are not that easy to print. What we do here is turn
  // each error into an easier-to-print data structure: a `PrintableError`
  // using a nice declarative API. So we can ignore all the gory details of how
  // that is actually printed and we do not have to make any effort to add and
  // print new errors.
  let printable_error = case error {
    PgCannotEstablishTcpConnection(host:, port:, reason:) -> {
      let mug_error = case reason {
        mug.ConnectFailedBoth(ipv4: error, ipv6: _)
        | mug.ConnectFailedIpv4(ipv4: error)
        | mug.ConnectFailedIpv6(ipv6: error) -> error
      }

      printable_error("Cannot establish TCP connection")
      |> add_paragraph(case mug_error {
        mug.Econnrefused ->
          "I couldn't connect to the database because "
          <> style_inline_code(host)
          <> " refused the connection to port "
          <> int.to_string(port)
          <> "."

        mug.Closed ->
          "I couldn't connect to the database because "
          <> style_inline_code(host)
          <> " closed the connection to port "
          <> int.to_string(port)
          <> "."

        mug.Ehostunreach ->
          "I couldn't connect to the database because "
          <> style_inline_code(host)
          <> " is unreachable."

        mug.Timeout ->
          "I couldn't connect to "
          <> style_inline_code(host)
          <> " at port "
          <> int.to_string(port)
          <> " because the connection timed out."

        _ ->
          "I couldn't connect to the database because I ran into the following
problem while trying to establish a TCP connection to
" <> style_inline_code(host) <> " at port " <> int.to_string(port) <> ":
" <> string.inspect(reason)
      })
    }

    PgInvalidUserDatabase(user:, database:) ->
      printable_error("Cannot connect")
      |> add_paragraph(
        "I couldn't connect to database "
        <> style_inline_code(database)
        <> " with user "
        <> style_inline_code(user)
        <> ".",
      )
      |> hint(
        "You can change the default user and database by setting the "
        <> style_inline_code("PGUSER")
        <> " and "
        <> style_inline_code("PGDATABASE")
        <> " environment variables.",
      )

    PgUnexpectedAuthMethodMessage(expected:, got:) ->
      printable_error("Cannot authenticate (no-method)")
      |> add_paragraph(
        "I ran into an unexpected problem while trying to authenticate with the
Postgres server. This is most definitely a bug!",
      )
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    PgInvalidPassword(user:) ->
      printable_error("Cannot authenticate")
      |> add_paragraph(
        "Invalid password for user " <> style_inline_code(user) <> ".",
      )
      |> hint("You can change the default password used to
authenticate by setting the " <> style_inline_code("PGPASSWORD") <> " environment variable.")

    PgUnexpectedCleartextAuthMessage(expected:, got:) ->
      printable_error("Cannot authenticate (cleartext)")
      |> add_paragraph(
        "I ran into an unexpected problem while trying to authenticate with the
Postgres server. This is most definitely a bug!",
      )
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    PgUnexpectedSha256AuthMessage(expected:, got:) ->
      printable_error("Cannot authenticate (sha256)")
      |> add_paragraph(
        "I ran into an unexpected problem while trying to authenticate with the
Postgres server. This is most definitely a bug!",
      )
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    PgInvalidSha256ServerProof ->
      printable_error("Cannot authenticate")
      |> add_paragraph("I couldn't authenticate with the Postgres server.")

    PgUnsupportedAuthentication(auth:) ->
      printable_error("Unsupported authentication method")
      |> add_paragraph(
        "The Postgres server is asking to authenticate using the "
        <> auth
        <> " authentication method, which I currently do not support.",
      )
      |> call_to_action(for: "this authentication method to be supported")

    PgCannotSendMessage(reason:) ->
      printable_error("Cannot send message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to talk to the Postgres
database server.",
      )
      |> report_bug(reason)

    PgCannotDecodeReceivedMessage(reason:) ->
      printable_error("Cannot decode message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to decode a message
received from the Postgres database server.",
      )
      |> report_bug(reason)

    PgCannotReceiveMessage(reason:) ->
      printable_error("Cannot receive message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to listen to the Postgres
database server.",
      )
      |> report_bug(reason)

    PgCannotDescribeQuery(file:, query_name:, expected:, got:) ->
      printable_error("Cannot inspect query")
      |> add_paragraph("I ran into an unexpected problem while trying to figure
out the types of query " <> style_inline_code(query_name) <> "
defined in " <> style_file(file) <> ". This is most definitely a bug!")
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    PgPermissionDenied(query_file:, reason:) ->
      printable_error("Permission denied")
      |> add_paragraph(
        "I cannot type the query defined in " <> style_link(query_file) <> "
because the server denied me permission with the following message: " <> reason <> ".",
      )
      |> hint(
        "Make sure the current user has the privileges to run this query.",
      )

    PgCannotExplainQuery(expected:, file:, got:, query_name:) ->
      printable_error("Cannot explain query")
      |> add_paragraph("I ran into an unexpected problem while trying to figure
out the types of query " <> style_inline_code(query_name) <> "
defined in " <> style_file(file) <> ". This is most definitely a bug!")
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    InvalidConnectionString(string:) ->
      printable_error("Invalid connection string")
      |> add_paragraph(
        "The value of the "
        <> style_inline_code("DATABASE_URL")
        <> " variable "
        <> style_inline_code(string)
        <> " is not a valid connection string.",
      )
      |> hint(
        "A connection string should have the following format: "
        <> style_inline_code(
          "postgres://username:password@host:port/database_name",
        ),
      )

    CannotReadFile(file:, reason:) ->
      printable_error("Cannot read file")
      |> add_paragraph(
        "I couldn't read "
        <> style_file(file)
        <> " because of the following error: "
        <> simplifile.describe_error(reason),
      )

    CannotWriteToFile(file:, reason:) ->
      printable_error("Cannot write to file")
      |> add_paragraph(
        "I couldn't write to "
        <> style_file(file)
        <> " because of the following error: "
        <> simplifile.describe_error(reason),
      )

    QueryFileHasInvalidName(file:, suggested_name:, reason: _) ->
      printable_error("Query file with invalid name")
      |> add_paragraph(
        "File " <> style_file(file) <> " doesn't have a valid name.
The name of a file is used to generate a corresponding Gleam function, so it
should be a valid Gleam name.",
      )
      |> hint("A file name must start with a lowercase letter and can only
contain lowercase letters, numbers and underscores." <> case suggested_name {
        Some(name) ->
          "\nMaybe try renaming it to " <> style_inline_code(name) <> "?"
        None -> ""
      })

    QueryHasInvalidColumn(
      file:,
      column_name:,
      suggested_name:,
      content:,
      reason:,
      starting_line:,
    ) ->
      case reason {
        ValueIsEmpty ->
          printable_error("Column with empty name")
          |> add_code_paragraph(file:, content:, point: None, starting_line:)
          |> add_paragraph(
            "A column returned by this query has the empty string as a name,
all columns should have a valid Gleam name as name.",
          )

        _ -> {
          let message = case suggested_name {
            None -> "This is not a valid Gleam name"
            Some(suggestion) ->
              "This is not a valid Gleam name, maybe try "
              <> style_inline_code(suggestion)
              <> "?"
          }

          printable_error("Column with invalid name")
          |> add_code_paragraph(
            file:,
            content:,
            starting_line:,
            point: Some(Pointer(point_to: Name(column_name), message:)),
          )
          |> hint(
            "A column name must start with a lowercase letter and can only
contain lowercase letters, numbers and underscores.",
          )
        }
      }

    QueryHasInvalidEnum(file:, content:, starting_line:, enum_name:, reason:) ->
      printable_error("Query with invalid enum")
      |> add_code_paragraph(file:, content:, point: None, starting_line:)
      |> add_paragraph(
        "One of the values in this query is the "
        <> style_inline_code(enum_name)
        <> " enum, but I cannot turn it into a Gleam type definition because "
        <> case reason {
          EnumWithNoVariants -> "it has no variants."
          InvalidEnumName(_) ->
            "its name cannot be turned into a valid type name."
          InvalidEnumVariants(fields) -> {
            let pretty_fields =
              list.map(fields, style_inline_code)
              |> string.join(with: ", ")

            "some of its possible values ("
            <> pretty_fields
            <> ") cannot be turned into valid type variants."
          }
        },
      )
      |> maybe_hint(case reason {
        InvalidEnumVariants(_) ->
          Some(
            "A valid enum variant must start with a letter and can only contain
letters, underscores and numbers. I will take care of automatically converting
any snake_case variant to PascalCase so that it can be used as a variant of a
Gleam type!",
          )
        InvalidEnumName(_) ->
          Some(
            "A valid enum name must start with a letter and can only contain
letters, underscores and numbers. I will take care automatically of converting
any snake_case name to PascalCase so that it can be used as the name of a
Gleam type!",
          )
        EnumWithNoVariants -> None
      })

    QueryHasUnsupportedType(file:, name: _, content:, type_:, starting_line:) -> {
      let base_error =
        printable_error("Unsupported type")
        |> add_code_paragraph(file:, content:, point: None, starting_line:)
        |> add_paragraph(
          "One of the rows returned by this query has type "
          <> style_inline_code(type_)
          <> " which I cannot currently generate code for.",
        )

      case type_ {
        // Timestampz usage is highly discouraged as it's only good if your
        // language is bad with time. In all other cases you're worse off using
        // it and should favour `timestamp`, so instead of a call to action to
        // ask for support we point people to an explanation on why this should
        // be avoided.
        "timestamptz" ->
          base_error
          |> hint(
            "In Postgres a "
            <> style_inline_code("timestamptz")
            <> " is converted to a regular "
            <> style_inline_code("timestamp")
            <> " using the connection's time zone. This is very error prone and
should be avoided in favour of using regular timestamps.",
          )

        _ ->
          base_error
          |> call_to_action(for: "this type to be supported")
      }
    }

    CannotParseQuery(
      file:,
      name: _,
      content:,
      starting_line:,
      error_code:,
      hint:,
      additional_error_message:,
      pointer:,
    ) -> {
      let error =
        printable_error(case error_code {
          Some(code) -> "Invalid query [" <> code <> "]"
          None -> "Invalid query"
        })
        |> add_code_paragraph(file:, content:, point: pointer, starting_line:)
        |> maybe_hint(hint)

      case additional_error_message {
        Some(message) -> add_paragraph(error, message)
        None -> error
      }
    }

    QueryReturnsMultipleValuesWithTheSameName(
      file:,
      content:,
      starting_line:,
      names:,
    ) -> {
      let pretty_names =
        list.map(names, style_inline_code) |> string.join(with: ", ")

      let name = case names {
        [] | [_, _, ..] -> "names"
        [_] -> "name"
      }

      printable_error("Duplicate names")
      |> add_code_paragraph(file:, content:, point: None, starting_line:)
      |> add_paragraph(
        "This query returns multiple values sharing the same "
        <> name
        <> ": "
        <> pretty_names
        <> ".",
      )
    }

    CannotParsePlanForQuery(file:, reason:) ->
      printable_error("Cannot decode query plan")
      |> add_paragraph(
        "I ran into an unexpected error while trying to figure out how to
generate code for query " <> style_file(file) <> ".",
      )
      |> report_bug(string.inspect(reason))

    PostgresVersionIsTooOld ->
      printable_error("Outdated Postgres version")
      |> add_paragraph("Squirrel only works with Postgres versions >= 16")
      |> add_paragraph(
        "If you have a good reason that's blocking you from upgrading Postgres
version and you want to use Squirrel, please open an issue at " <> style_link(
          "https://github.com/giacomocavalieri/squirrel/issues/new",
        ),
      )

    PostgresVersionHasInvalidFormat(invalid_version) ->
      printable_error("Postgres version with unexpected format")
      |> add_paragraph(
        "It looks like your Postgres server's version has an unexpected format.
This is most definitely a bug!",
      )
      |> report_bug(bit_array.inspect(invalid_version))

    OutdatedFile(file:) ->
      printable_error("Outdated file")
      |> add_paragraph(
        "It looks like "
        <> style_file(file)
        <> " is outdated, try running `gleam run -m squirrel` to generate a new
up to date version.",
      )

    CannotOverwriteExistingFile(file:) ->
      printable_error("Cannot overwrite file")
      |> add_paragraph(
        "It looks like " <> style_file(file) <> " already exists and was not
generated by Squirrel, I cannot overwrite it!",
      )
      |> hint("Rename the file and run `gleam run -m squirrel` again.")
  }

  printable_error_to_doc(printable_error)
}

fn style_file(file: String) -> String {
  ansi.underline(file)
}

fn style_inline_code(code: String) -> String {
  "`" <> code <> "`"
}

fn style_link(link: String) -> String {
  ansi.underline(link)
}

// --- ERROR PRETTY PRINTING ---------------------------------------------------

const indent = 2

type PrintableError {
  PrintableError(
    title: String,
    body: List(Paragraph),
    report_bug: Option(String),
    call_to_action: Option(String),
    hint: Option(String),
  )
}

type Paragraph {
  Simple(String)
  Code(
    file: String,
    content: String,
    pointer: Option(Pointer),
    starting_line: Int,
  )
}

/// A default printable error with just a title.
///
fn printable_error(title: String) -> PrintableError {
  PrintableError(
    title:,
    body: [],
    report_bug: None,
    hint: None,
    call_to_action: None,
  )
}

fn add_paragraph(error: PrintableError, string: String) -> PrintableError {
  PrintableError(..error, body: list.append(error.body, [Simple(string)]))
}

fn add_code_paragraph(
  error: PrintableError,
  file file: String,
  content content: String,
  point point: Option(Pointer),
  starting_line starting_line: Int,
) -> PrintableError {
  PrintableError(
    ..error,
    body: list.append(error.body, [
      Code(file:, content:, pointer: point, starting_line:),
    ]),
  )
}

/// Sets a call to action to report a specific bug.
///
fn report_bug(error: PrintableError, report_bug: String) -> PrintableError {
  PrintableError(..error, report_bug: Some(report_bug))
}

/// Sets a hint that will be displayed at the bottom of the error message.
///
fn hint(error: PrintableError, hint: String) -> PrintableError {
  PrintableError(..error, hint: Some(hint))
}

/// Sets a hint that will be displayed at the bottom of the error message.
///
fn maybe_hint(error: PrintableError, hint: Option(String)) -> PrintableError {
  PrintableError(..error, hint:)
}

/// Given something a user might want to be added to the package it sets a
/// call to action message telling someone to open a ticket on the `squirrel`
/// repo.
///
fn call_to_action(error: PrintableError, for wanted: String) -> PrintableError {
  PrintableError(..error, call_to_action: Some(wanted))
}

fn printable_error_to_doc(error: PrintableError) -> Document {
  // And now for the tricky bit...
  let PrintableError(title:, body:, report_bug:, call_to_action:, hint:) = error

  [
    title_doc(title),
    body_doc(body),
    option_to_doc(report_bug, report_bug_doc),
    option_to_doc(call_to_action, call_to_action_doc),
    option_to_doc(hint, hint_doc),
  ]
  |> list.filter(keeping: fn(doc) { doc != doc.empty })
  |> doc.join(with: doc.lines(2))
  |> doc.group
}

fn title_doc(title: String) -> Document {
  doc.from_string(ansi.red(ansi.bold("Error: ") <> title))
}

fn body_doc(body: List(Paragraph)) -> Document {
  list.map(body, paragraph_doc)
  |> doc.join(with: doc.lines(2))
  |> doc.group
}

fn paragraph_doc(paragraph: Paragraph) -> Document {
  case paragraph {
    Simple(string) -> flexible_string(string)
    Code(file:, content:, pointer:, starting_line:) ->
      code_doc(file:, content:, pointer:, starting_line:)
  }
}

fn code_doc(
  file file: String,
  content content: String,
  pointer pointer: Option(Pointer),
  starting_line starting_line: Int,
) {
  let pointer =
    option.to_result(pointer, Nil)
    |> result.try(pointer_doc(_, content))

  let content = syntax_highlight(content)
  let lines = string.split(content, on: "\n")
  let lines_count = list.length(lines)
  let digits = digits(lines_count + starting_line)
  let max_digits = list.length(digits)

  let code_lines = {
    use line, i <- list.index_map(lines)
    let prefix =
      int.to_string(i + starting_line)
      |> string.pad_start(to: max_digits + 2, with: " ")

    case pointer {
      Ok(#(pointer_line, from, pointer_doc)) if pointer_line == i -> [
        doc.from_string(ansi.dim(prefix <> " │ ")),
        doc.from_string(line),
        [doc.line, pointer_doc]
          |> doc.concat
          |> doc.nest(by: from + max_digits + 5),
      ]

      Ok(_) -> [
        doc.from_string(ansi.dim(prefix <> " │ ")),
        doc.from_string(ansi.dim(line)),
      ]

      // If there's no pointer whatsoever do not dim any line at all, or
      // everything would be dimmed!
      Error(_) -> [doc.from_string(ansi.dim(prefix <> " │ ") <> line)]
    }
    |> doc.concat
  }

  let padding = string.repeat(" ", max_digits + 3)
  [
    doc.from_string(padding <> ansi.dim("╭─ " <> file)),
    case starting_line {
      1 -> doc.from_string(padding <> ansi.dim("│"))
      _ -> doc.from_string(padding <> ansi.dim("┆"))
    },
    ..code_lines
  ]
  |> doc.join(with: doc.line)
  |> doc.append(doc.line)
  |> doc.append(doc.from_string(padding <> ansi.dim("┆")))
  |> doc.group
}

fn digits(int: Int) -> List(Int) {
  digits_loop(int.absolute_value(int), [])
}

fn digits_loop(int: Int, acc: List(Int)) -> List(Int) {
  case int < 10 {
    True -> [int, ..acc]
    False -> digits_loop(int / 10, [int % 10, ..acc])
  }
}

fn pointer_doc(
  pointer: Pointer,
  content: String,
) -> Result(#(Int, Int, Document), Nil) {
  let Pointer(kind, message) = pointer
  use #(line, from, to) <- result.try(find_span(kind, content))
  let width = to - from + 1
  let doc =
    [
      doc.zero_width_string("\u{001B}[31m"),
      doc.from_string("┬" <> string.repeat("─", width - 1)),
      doc.line,
      doc.from_string("╰─ "),
      flexible_string(message)
        |> doc.nest(by: 3),
      doc.zero_width_string("\u{001B}[0m"),
    ]
    |> doc.concat
    |> doc.group

  Ok(#(line, from, doc))
}

fn find_span(kind: PointerKind, string: String) -> Result(#(Int, Int, Int), Nil) {
  case kind {
    Name(name) -> find_name_span(name, string.length(name), string, 0, 0)
    ByteIndex(n) -> find_byte_span(n - 1, string, 0, 0)
  }
}

fn find_name_span(
  name: String,
  name_len: Int,
  string: String,
  row: Int,
  col: Int,
) -> Result(#(Int, Int, Int), Nil) {
  case string.starts_with(string, name) {
    True -> Ok(#(row, col, col + name_len - 1))
    False ->
      case string.pop_grapheme(string) {
        Ok(#("\n", rest)) -> find_name_span(name, name_len, rest, row + 1, 0)
        Ok(#(_, rest)) -> find_name_span(name, name_len, rest, row, col + 1)
        Error(_) -> Error(Nil)
      }
  }
}

fn find_byte_span(
  position: Int,
  string: String,
  row: Int,
  col: Int,
) -> Result(#(Int, Int, Int), Nil) {
  case position {
    0 -> Ok(#(row, col, col))
    n ->
      case string.pop_grapheme(string) {
        Ok(#("\n", rest)) -> find_byte_span(n - 1, rest, row + 1, 0)
        Ok(#(_, rest)) -> find_byte_span(n - 1, rest, row, col + 1)
        Error(_) -> Error(Nil)
      }
  }
}

const keywords = [
  "and", "any", "as", "asc", "begin", "between", "by", "case", "commit",
  "conflict", "constraint", "count", "desc", "distinct", "do", "drop", "else",
  "end", "exists", "from", "full", "group", "having", "if", "in", "inner",
  "insert", "into", "join", "key", "left", "like", "not", "nothing", "null",
  "on", "or", "order", "primary", "revert", "right", "select", "set", "table",
  "top", "trigger", "union", "update", "use", "values", "view", "where", "with",
]

fn syntax_highlight(content: String) -> String {
  let keywords = string.join(keywords, with: "|")
  let not_inside_string = "(?=(?:[^']*'[^']*')*[^']*$)"

  let assert Ok(keyword) =
    { "\\b(" <> keywords <> ")\\b" <> not_inside_string }
    |> regexp.compile(regexp.Options(True, False))
  let assert Ok(number) =
    regexp.from_string("(?<!\\$)\\b(\\d+(\\.\\d+)?\\b)" <> not_inside_string)
  let assert Ok(comment) = regexp.from_string("(^\\s*--.*)")
  let assert Ok(string) = regexp.from_string("(\\'.*\\')")
  let assert Ok(hole) = regexp.from_string("(\\$\\d+)" <> not_inside_string)

  content
  |> regexp.replace(each: comment, with: "\u{001B}[2m\\1\u{001B}[0m")
  |> regexp.replace(each: keyword, with: "\u{001B}[36m\\1\u{001B}[39m")
  |> regexp.replace(each: string, with: "\u{001B}[33m\\1\u{001B}[39m")
  |> regexp.replace(each: number, with: "\u{001B}[32m\\1\u{001B}[39m")
  |> regexp.replace(each: hole, with: "\u{001B}[35m\\1\u{001B}[39m")
}

fn report_bug_doc(additional_info: String) -> Document {
  [
    flexible_string(
      "Please open an issue at "
      <> style_link("https://github.com/giacomocavalieri/squirrel/issues/new")
      <> " with some details about what you where doing, including the following message:",
    ),
    doc.line |> doc.nest(by: indent),
    doc.from_string(additional_info),
  ]
  |> doc.concat
  |> doc.group
}

fn call_to_action_doc(wanted: String) -> Document {
  flexible_string(
    "If you would like for "
    <> wanted
    <> ", please open an issue at "
    <> style_link("https://github.com/giacomocavalieri/squirrel/issues/new"),
  )
}

fn hint_doc(hint: String) -> Document {
  flexible_string("Hint: " <> hint)
}

fn flexible_string(string: String) -> Document {
  string.split(string, on: "\n")
  |> list.flat_map(string.split(_, on: " "))
  |> list.map(doc.from_string)
  |> doc.join(with: doc.flex_space)
  |> doc.group
}

fn option_to_doc(option: Option(a), fun: fn(a) -> Document) -> Document {
  case option {
    Some(a) -> fun(a)
    None -> doc.empty
  }
}
