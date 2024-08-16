import filepath
import glam/doc.{type Document}
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import simplifile
import squirrel/internal/error.{
  type Error, CannotReadFile, QueryFileHasInvalidName,
}
import squirrel/internal/gleam

/// A query that still needs to go through the type checking process.
///
pub type UntypedQuery {
  UntypedQuery(
    /// The file the query comes from.
    ///
    file: String,
    /// The starting line in the source file where the query is defined.
    ///
    starting_line: Int,
    /// The name of the query, it must be a valid Gleam identifier.
    ///
    name: gleam.ValueIdentifier,
    /// Any comment lines that were preceding the query in the file.
    ///
    comment: List(String),
    /// The text of the query itself.
    ///
    content: String,
  )
}

/// This is exactly the same as an untyped query with the difference that it
/// has also been annotated with the type of its parameters and returned values.
///
pub type TypedQuery {
  TypedQuery(
    file: String,
    starting_line: Int,
    name: gleam.ValueIdentifier,
    comment: List(String),
    content: String,
    params: List(gleam.Type),
    returns: List(gleam.Field),
  )
}

/// Turns an untyped query into a typed one.
///
pub fn add_types(
  to query: UntypedQuery,
  params params: List(gleam.Type),
  returns returns: List(gleam.Field),
) -> TypedQuery {
  let UntypedQuery(
    file: file,
    name: name,
    comment: comment,
    content: content,
    starting_line: starting_line,
  ) = query
  TypedQuery(
    file: file,
    name: name,
    comment: comment,
    content: content,
    starting_line: starting_line,
    params: params,
    returns: returns,
  )
}

// --- PARSING -----------------------------------------------------------------

/// Reads a query from a file.
/// This expects the user to follow the convention of having a single query per
/// file.
///
pub fn from_file(file: String) -> Result(UntypedQuery, Error) {
  let read_file =
    simplifile.read(file)
    |> result.map_error(CannotReadFile(file, _))

  use content <- result.try(read_file)

  // A query always starts at the top of the file.
  // If in the future I want to add support for many queries per file this
  // field will be handy to properly show error messages.
  let file_name =
    filepath.base_name(file)
    |> filepath.strip_extension
  let name =
    gleam.identifier(file_name)
    |> result.map_error(QueryFileHasInvalidName(
      file: file,
      reason: _,
      suggested_name: gleam.similar_identifier_string(file_name)
        |> option.from_result,
    ))

  use name <- result.try(name)
  Ok(UntypedQuery(
    file: file,
    starting_line: 1,
    name: name,
    content: content,
    comment: take_comment(content),
  ))
}

fn take_comment(query: String) -> List(String) {
  do_take_comment(query, [])
}

fn do_take_comment(query: String, lines: List(String)) -> List(String) {
  case string.trim_left(query) {
    "--" <> rest ->
      case string.split_once(rest, on: "\n") {
        Ok(#(line, rest)) -> do_take_comment(rest, [string.trim(line), ..lines])
        _ -> do_take_comment("", [string.trim(rest), ..lines])
      }
    _ -> list.reverse(lines)
  }
}

// --- CODE GENERATION ---------------------------------------------------------

const indent = 2

fn gleam_type_to_decoder(
  state: CodeGenState,
  type_: gleam.Type,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.Int -> #(state, doc.from_string("decode.int"))
    gleam.Float -> #(state, doc.from_string("decode.float"))
    gleam.Bool -> #(state, doc.from_string("decode.bool"))
    gleam.String -> #(state, doc.from_string("decode.string"))
    gleam.Json -> #(state, doc.from_string("decode.string"))

    gleam.List(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.list", [inner_decoder]))
    }

    gleam.Option(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.optional", [inner_decoder]))
    }
  }
}

fn gleam_type_to_encoder(
  state: CodeGenState,
  type_: gleam.Type,
  name: String,
) -> #(CodeGenState, Document) {
  let name = doc.from_string(name)
  case type_ {
    gleam.List(type_) -> {
      let state = CodeGenState(..state, used_list: True)
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let map_fn = fn_doc(["value"], inner_encoder)
      let doc = call_doc("pgo.array", [call_doc("list.map", [name, map_fn])])

      #(state, doc)
    }

    gleam.Option(type_) -> {
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let doc =
        call_doc("pgo.nullable", [fn_doc(["value"], inner_encoder), name])

      #(state, doc)
    }

    gleam.Json -> {
      let state = CodeGenState(..state, used_json: True)
      let doc = call_doc("pgo.text", [call_doc("json.to_string", [name])])
      #(state, doc)
    }

    gleam.Int -> #(state, call_doc("pgo.int", [name]))
    gleam.Float -> #(state, call_doc("pgo.float", [name]))
    gleam.Bool -> #(state, call_doc("pgo.bool", [name]))
    gleam.String -> #(state, call_doc("pgo.text", [name]))
  }
}

fn gleam_type_to_field_type(
  state: CodeGenState,
  type_: gleam.Type,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.List(type_) -> {
      let #(state, inner_type) = gleam_type_to_field_type(state, type_)
      #(state, call_doc("List", [inner_type]))
    }
    gleam.Option(type_) -> {
      let state = CodeGenState(..state, used_option_type: True)
      let #(state, inner_type) = gleam_type_to_field_type(state, type_)
      #(state, call_doc("Option", [inner_type]))
    }
    gleam.Int -> #(state, doc.from_string("Int"))
    gleam.Float -> #(state, doc.from_string("Float"))
    gleam.Bool -> #(state, doc.from_string("Bool"))
    gleam.String -> #(state, doc.from_string("String"))
    gleam.Json -> #(state, doc.from_string("String"))
  }
}

type CodeGenState {
  CodeGenState(used_list: Bool, used_option_type: Bool, used_json: Bool)
}

/// Generates the code for a single file containing a bunch of typed queries.
///
pub fn generate_code(queries: List(TypedQuery), version: String) -> String {
  let #(state, docs) = {
    let state =
      CodeGenState(used_list: False, used_option_type: False, used_json: False)
    use #(state, docs), query <- list.fold(over: queries, from: #(state, []))
    let #(state, doc) = query_doc(state, version, query)
    #(state, [doc, ..docs])
  }

  let CodeGenState(
    used_list: used_list,
    used_option_type: used_option_type,
    used_json: used_json,
  ) = state

  let imports =
    ["import gleam/pgo", "import decode"]
    |> append_if(used_list, "import gleam/list")
    |> append_if(used_json, "import gleam/json")
    |> append_if(used_option_type, "import gleam/option.{type Option}")
    |> list.sort(string.compare)
    |> list.map(doc.from_string)
    |> doc.join(with: doc.line)

  [imports, doc.lines(2), doc.join(docs, with: doc.lines(2))]
  |> doc.concat
  |> doc.to_string(80)
}

/// Returns the generated code and a set with the needed imports to make it
/// compile.
///
fn query_doc(
  state: CodeGenState,
  version: String,
  query: TypedQuery,
) -> #(CodeGenState, Document) {
  let TypedQuery(
    file: _,
    name: name,
    content: content,
    comment: _,
    params: params,
    returns: returns,
    starting_line: _,
  ) = query

  let constructor_name = gleam.identifier_to_type_name(name) <> "Row"
  let record_result = record_doc(state, version, constructor_name, query)
  let #(state, record) = case record_result {
    Ok(#(state, record)) -> #(state, doc.append(record, doc.lines(2)))
    Error(_) -> #(state, doc.empty)
  }

  let #(state, inputs, encoders) = {
    let acc = #(state, [], [])
    use acc, param, i <- list.index_fold(params, acc)
    let #(state, inputs, encoders) = acc

    let input = "arg_" <> int.to_string(i + 1)
    let #(state, encoder) = gleam_type_to_encoder(state, param, input)
    #(state, [input, ..inputs], [encoder, ..encoders])
  }
  let inputs = list.reverse(inputs)
  let encoders = list.reverse(encoders)

  let #(state, decoder) = decoder_doc(state, constructor_name, returns)
  let code =
    [
      record,
      doc.from_string(function_doc(version, query)),
      doc.line,
      fun_doc(gleam.identifier_to_string(name), ["db", ..inputs], [
        var_doc("decoder", decoder),
        pipe_call_doc("pgo.execute", string_doc(content), [
          doc.from_string("db"),
          list_doc(encoders),
          doc.from_string("decode.from(decoder, _)"),
        ]),
      ]),
    ]
    |> doc.concat

  #(state, code)
}

fn function_doc(version: String, query: TypedQuery) -> String {
  let TypedQuery(comment: comment, name: name, file: file, ..) = query
  let function_name = gleam.identifier_to_string(name)

  let base = case comment {
    [] -> "/// Runs the `" <> function_name <> "` query
/// defined in `" <> file <> "`."
    [_, ..] ->
      list.map(comment, string.append("/// ", _))
      |> string.join(with: "\n")
  }

  base <> "
///
/// > 🐿️ This function was generated automatically using " <> version <> " of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"
}

/// Returns the document of a record type definition if the query warrants its
/// creation: if a query doesn't return anything, then it doesn't make sense
/// to create a new record type and this function will return an `Error`.
///
/// Otherwise it returns the document defining a commented type definition with
/// the name passed in as a parameter.
///
fn record_doc(
  state: CodeGenState,
  version: String,
  type_name: String,
  query: TypedQuery,
) -> Result(#(CodeGenState, Document), Nil) {
  let TypedQuery(name: name, returns: returns, file: file, ..) = query
  use <- bool.guard(when: returns == [], return: Error(Nil))

  let function_name = gleam.identifier_to_string(name)
  let record_doc =
    "/// A row you get from running the `" <> function_name <> "` query
/// defined in `" <> file <> "`.
///
/// > 🐿️ This type definition was generated automatically using " <> version <> " of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"

  let #(state, fields) = {
    use #(state, fields), field <- list.fold(returns, from: #(state, []))
    let label = doc.from_string(gleam.identifier_to_string(field.label) <> ": ")
    let #(state, field_type) = gleam_type_to_field_type(state, field.type_)
    let field = [label, field_type] |> doc.concat |> doc.group

    #(state, [field, ..fields])
  }
  let fields = list.reverse(fields)

  let result =
    [
      doc.from_string(record_doc),
      doc.line,
      [
        doc.from_string("pub type " <> type_name <> " {"),
        [doc.line, call_doc(type_name, fields)]
          |> doc.concat
          |> doc.nest(by: indent),
        doc.line,
        doc.from_string("}"),
      ]
        |> doc.concat
        |> doc.group,
    ]
    |> doc.concat

  Ok(#(state, result))
}

/// A decoder that discards its value and always returns `Nil` instead.
///
const nil_decoder = "decode.map(decode.dynamic, fn(_) { Nil })"

/// A pretty printed decoder that decodes an n-item dynamic tuple using the
/// `decode` package.
///
fn decoder_doc(
  state: CodeGenState,
  constructor: String,
  returns: List(gleam.Field),
) -> #(CodeGenState, Document) {
  let fallback = #(state, doc.from_string(nil_decoder))
  use <- bool.guard(when: returns == [], return: fallback)

  let #(state, parameters, labelled_names, pipes) = {
    let acc = #(state, [], [], [])
    use acc, field, i <- list.index_fold(returns, acc)
    let #(state, parameters, labelled_names, pipes) = acc

    let label = gleam.identifier_to_string(field.label)
    let param = doc.from_string("use " <> label <> " <- decode.parameter")
    let parameters = [param, ..parameters]

    let labelled_name = doc.from_string(label <> ": " <> label)
    let labelled_names = [labelled_name, ..labelled_names]

    let position = int.to_string(i) |> doc.from_string
    let #(state, decoder) = gleam_type_to_decoder(state, field.type_)
    let pipe = call_doc("|> decode.field", [position, decoder])
    let pipes = [pipe, ..pipes]

    #(state, parameters, labelled_names, pipes)
  }
  let parameters = list.reverse(parameters)
  let pipes = list.reverse(pipes)
  let labelled_names = list.reverse(labelled_names)

  let doc =
    [
      call_block("decode.into", [
        doc.join(parameters, with: doc.line),
        doc.line,
        call_doc(constructor, labelled_names),
      ]),
      doc.line,
      doc.join(pipes, with: doc.line),
    ]
    |> doc.concat()
    |> doc.group

  #(state, doc)
}

/// A pretty printed function call where the first argument is piped into
/// the function.
///
fn pipe_call_doc(
  function: String,
  first: Document,
  rest: List(Document),
) -> Document {
  [first, doc.line, call_doc("|> " <> function, rest)]
  |> doc.concat
}

/// A pretty printed function call.
///
fn call_doc(function: String, args: List(Document)) -> Document {
  [doc.from_string(function), comma_list("(", args, ")")]
  |> doc.concat
  |> doc.group
}

/// A pretty printed function call where the only argument is a single block.
///
fn call_block(function: String, body: List(Document)) -> Document {
  [doc.from_string(function <> "("), block(body), doc.from_string(")")]
  |> doc.concat
  |> doc.group
}

/// A pretty printed Gleam block.
///
fn block(body: List(Document)) -> Document {
  [
    doc.from_string("{"),
    [doc.line, ..body]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.force_break
}

/// A pretty printed public function definition.
///
fn fun_doc(name: String, args: List(String), body: List(Document)) -> Document {
  let args = list.map(args, doc.from_string)

  [
    doc.from_string("pub fn " <> name),
    comma_list("(", args, ") "),
    block([body |> doc.join(with: doc.lines(2))]),
    doc.line,
  ]
  |> doc.concat
  |> doc.group
}

fn fn_doc(args: List(String), body: Document) -> Document {
  [
    doc.from_string("fn"),
    comma_list("(", list.map(args, doc.from_string), ") {"),
    [doc.space, body]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.space,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.group
}

/// A pretty printed let assignment.
///
fn var_doc(name: String, body: Document) -> Document {
  [
    doc.from_string("let " <> name <> " ="),
    [doc.space, body]
      |> doc.concat
      |> doc.group
      |> doc.nest(by: indent),
  ]
  |> doc.concat
}

/// A pretty printed Gleam string.
///
/// > ⚠️ This function escapes all `\` and `"` inside the original string to
/// > avoid generating invalid Gleam code.
///
fn string_doc(content: String) -> Document {
  let escaped_string =
    content
    |> string.replace(each: "\\", with: "\\\\")
    |> string.replace(each: "\"", with: "\\\"")
    |> doc.from_string

  [doc.from_string("\""), escaped_string, doc.from_string("\"")]
  |> doc.concat
}

/// A pretty printed Gleam list.
///
fn list_doc(elems: List(Document)) -> Document {
  comma_list("[", elems, "]")
}

/// A comma separated list of items with some given open and closed delimiters.
///
fn comma_list(open: String, content: List(Document), close: String) -> Document {
  [
    doc.from_string(open),
    [
      // We want the first break to be nested
      // in case the group is broken.
      doc.soft_break,
      doc.join(content, doc.break(", ", ",")),
    ]
      |> doc.concat
      |> doc.group
      |> doc.nest(by: indent),
    doc.break("", ","),
    doc.from_string(close),
  ]
  |> doc.concat
  |> doc.group
}

// --- UTILS -------------------------------------------------------------------

fn append_if(list: List(a), cond: Bool, value: a) -> List(a) {
  case cond {
    True -> [value, ..list]
    False -> list
  }
}
