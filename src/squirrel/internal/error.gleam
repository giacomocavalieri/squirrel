import glam/doc.{type Document}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regex
import gleam/result
import gleam/string
import gleam_community/ansi
import simplifile

pub type Error {
  // --- POSTGRES RELATED ERRORS -----------------------------------------------
  /// When authentication workflow goes wrong.
  /// TODO)) For now I only support no authentication so people might report
  /// this issue.
  ///
  PgCannotAuthenticate(expected: String, got: String)

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

  // --- OTHER GENERIC ERRORS --------------------------------------------------
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

  /// If the query contains an error and cannot be parsed by the DBMS.
  ///
  CannotParseQuery(
    file: String,
    name: String,
    content: String,
    starting_line: Int,
    error_code: Option(String),
    pointer: Option(Pointer),
    hint: Option(String),
  )
}

pub type ValueIdentifierError {
  DoesntStartWithLowercaseLetter
  ContainsInvalidGrapheme(at: Int, grapheme: String)
  IsEmpty
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
    PgCannotSendMessage(reason: reason) ->
      printable_error("Cannot send message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to talk to the Postgres
database server.",
      )
      |> report_bug(reason)

    PgCannotDecodeReceivedMessage(reason: reason) ->
      printable_error("Cannot decode message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to decode a message
received from the Postgres database server.",
      )
      |> report_bug(reason)

    PgCannotReceiveMessage(reason: reason) ->
      printable_error("Cannot receive message")
      |> add_paragraph(
        "I ran into an unexpected error while trying to listen to the Postgres
database server.",
      )
      |> report_bug(reason)

    CannotReadFile(file: file, reason: reason) ->
      printable_error("Cannot read file")
      |> add_paragraph(
        "I couldn't read "
        <> style_file(file)
        <> " because of the following error: "
        <> simplifile.describe_error(reason),
      )

    CannotWriteToFile(file: file, reason: reason) ->
      printable_error("Cannot write to file")
      |> add_paragraph(
        "I couldn't write to "
        <> style_file(file)
        <> " because of the following error: "
        <> simplifile.describe_error(reason),
      )

    QueryFileHasInvalidName(
      file: file,
      suggested_name: suggested_name,
      reason: _,
    ) ->
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
      file: file,
      column_name: column_name,
      suggested_name: suggested_name,
      content: content,
      reason: reason,
      starting_line: starting_line,
    ) ->
      case reason {
        IsEmpty ->
          printable_error("Column with empty name")
          |> add_code_paragraph(
            file: file,
            content: content,
            point: None,
            starting_line: starting_line,
          )
          |> add_paragraph(
            "A column returned by this query has the empty string as a name,
all columns should have a valid Gleam name as name.",
          )

        _ ->
          printable_error("Column with invalid name")
          |> add_code_paragraph(
            file: file,
            content: content,
            starting_line: starting_line,
            point: Some(
              Pointer(point_to: Name(column_name), message: case
                suggested_name
              {
                None -> "This is not a valid Gleam name"
                Some(suggestion) ->
                  "This is not a valid Gleam name, maybe try "
                  <> style_inline_code(suggestion)
                  <> "?"
              }),
            ),
          )
          |> hint(
            "A column name must start with a lowercase letter and can only
contain lowercase letters, numbers and underscores.",
          )
      }

    QueryHasUnsupportedType(
      file: file,
      name: _,
      content: content,
      type_: type_,
      starting_line: starting_line,
    ) ->
      printable_error("Unsupported type")
      |> add_code_paragraph(
        file: file,
        content: content,
        point: None,
        starting_line: starting_line,
      )
      |> add_paragraph(
        "One of the rows returned by this query has type "
        <> style_inline_code(type_)
        <> " which I cannot currently generate code for.",
      )
      |> call_to_action(for: "this type to be supported")

    CannotParseQuery(
      file: file,
      name: _name,
      content: content,
      starting_line: starting_line,
      error_code: error_code,
      hint: hint,
      pointer: pointer,
    ) ->
      printable_error(case error_code {
        Some(code) -> "Invalid query [" <> code <> "]"
        None -> "Invalid query"
      })
      |> add_code_paragraph(
        file: file,
        content: content,
        point: pointer,
        starting_line: starting_line,
      )
      |> maybe_hint(hint)

    PgCannotAuthenticate(expected: expected, got: got) ->
      printable_error("Cannot authenticate")
      |> add_paragraph(
        "I ran into an unexpected problem while trying to authenticate with the
Postgres server. This is most definitely a bug!",
      )
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)

    PgCannotDescribeQuery(
      file: file,
      query_name: query_name,
      expected: expected,
      got: got,
    ) ->
      printable_error("Cannot inspect query")
      |> add_paragraph("I ran into an unexpected problem while trying to figure
out the types of query " <> style_inline_code(query_name) <> "
defined in " <> style_file(file) <> ". This is most definitely a bug!")
      |> report_bug("Expected: " <> expected <> ", Got: " <> got)
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
    title: title,
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
      Code(
        file: file,
        content: content,
        pointer: point,
        starting_line: starting_line,
      ),
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
  PrintableError(..error, hint: hint)
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
  let PrintableError(
    title: title,
    body: body,
    report_bug: report_bug,
    call_to_action: call_to_action,
    hint: hint,
  ) = error

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
  |> doc.join(with: doc.line)
  |> doc.group
}

fn paragraph_doc(paragraph: Paragraph) -> Document {
  case paragraph {
    Simple(string) -> flexible_string(string)
    Code(
      file: file,
      content: content,
      pointer: pointer,
      starting_line: starting_line,
    ) ->
      code_doc(
        file: file,
        content: content,
        pointer: pointer,
        starting_line: starting_line,
      )
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
    |> result.then(pointer_doc(_, content))

  let content = syntax_highlight(content)
  let lines = string.split(content, on: "\n")
  let lines_count = list.length(lines)
  let assert Ok(digits) = int.digits(lines_count + starting_line, 10)
  let max_digits = list.length(digits)

  let code_lines = {
    use line, i <- list.index_map(lines)
    let prefix =
      int.to_string(i + starting_line)
      |> string.pad_left(to: max_digits + 2, with: " ")

    case pointer {
      Ok(#(pointer_line, from, pointer_doc)) if pointer_line == i -> [
        doc.from_string(ansi.dim(prefix <> " │ ")),
        doc.from_string(line),
        [doc.line, pointer_doc]
          |> doc.concat
          |> doc.nest(by: from + max_digits + 5),
      ]

      Ok(_) | Error(_) -> [
        doc.from_string(ansi.dim(prefix <> " │ ")),
        doc.from_string(ansi.dim(line)),
      ]
    }
    |> doc.concat
  }

  let padding = string.repeat(" ", max_digits + 3)
  [
    doc.from_string(padding <> ansi.dim("╭─ " <> file)),
    case starting_line {
      1 -> doc.from_string(padding <> ansi.dim("│  "))
      _ -> doc.from_string(padding <> ansi.dim("┆  "))
    },
    ..code_lines
  ]
  |> doc.join(with: doc.line)
  |> doc.append(doc.line)
  |> doc.append(doc.from_string(padding <> ansi.dim("┆")))
  |> doc.group
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
  "and", "any", "as", "asc", "begin", "between", "by", "case", "count", "desc",
  "distinct", "else", "end", "exists", "from", "full", "group", "having", "if",
  "in", "inner", "insert", "into", "join", "key", "left", "like", "not", "null",
  "on", "or", "order", "primary", "revert", "right", "select", "set", "table",
  "top", "trigger", "union", "update", "use", "values", "view", "where", "with",
]

fn syntax_highlight(content: String) -> String {
  let keywords = string.join(keywords, with: "|")
  let not_inside_string = "(?=(?:[^']*'[^']*')*[^']*$)"

  let assert Ok(keyword) =
    { "\\b(" <> keywords <> ")\\b" <> not_inside_string }
    |> regex.compile(regex.Options(True, False))
  let assert Ok(number) =
    regex.from_string("(?<!\\$)\\b(\\d+(\\.\\d+)?\\b)" <> not_inside_string)
  let assert Ok(comment) = regex.from_string("(^\\s*--.*)")
  let assert Ok(string) = regex.from_string("(\\'.*\\')")
  let assert Ok(hole) = regex.from_string("(\\$\\d+)" <> not_inside_string)

  content
  |> regex.replace(each: comment, with: "\u{001B}[2m\\1\u{001B}[0m")
  |> regex.replace(each: keyword, with: "\u{001B}[36m\\1\u{001B}[39m")
  |> regex.replace(each: string, with: "\u{001B}[33m\\1\u{001B}[39m")
  |> regex.replace(each: number, with: "\u{001B}[32m\\1\u{001B}[39m")
  |> regex.replace(each: hole, with: "\u{001B}[35m\\1\u{001B}[39m")
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
