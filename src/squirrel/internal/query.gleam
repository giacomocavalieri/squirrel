import filepath
import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
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
  let UntypedQuery(file:, name:, comment:, content:, starting_line:) = query
  TypedQuery(
    file:,
    name:,
    comment:,
    content:,
    starting_line:,
    params:,
    returns:,
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
      file:,
      reason: _,
      suggested_name: gleam.similar_identifier_string(file_name)
        |> option.from_result,
    ))

  use name <- result.try(name)
  Ok(UntypedQuery(
    file:,
    starting_line: 1,
    name:,
    content:,
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

type CodeGenState {
  CodeGenState(
    imports: Dict(String, Set(String)),
    needs_uuid_decoder: Bool,
    needs_date_decoder: Bool,
    needs_timestamp_decoder: Bool,
  )
}

fn default_codegen_state() {
  CodeGenState(
    imports: dict.new(),
    needs_uuid_decoder: False,
    needs_date_decoder: False,
    needs_timestamp_decoder: False,
  )
  |> import_module("decode")
  |> import_module("gleam/pgo")
}

fn gleam_type_to_decoder(
  state: CodeGenState,
  type_: gleam.Type,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.Uuid -> {
      let state =
        CodeGenState(..state, needs_uuid_decoder: True)
        |> import_module("youid/uuid")
      #(state, doc.from_string("uuid_decoder()"))
    }
    gleam.List(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.list", [inner_decoder]))
    }
    gleam.Option(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.optional", [inner_decoder]))
    }
    gleam.Date -> {
      let state = CodeGenState(..state, needs_date_decoder: True)
      #(state, doc.from_string("date_decoder()"))
    }
    gleam.Timestamp -> {
      let state = CodeGenState(..state, needs_timestamp_decoder: True)
      #(state, doc.from_string("timestamp_decoder()"))
    }
    gleam.Int -> #(state, doc.from_string("decode.int"))
    gleam.Float -> #(state, doc.from_string("decode.float"))
    gleam.Bool -> #(state, doc.from_string("decode.bool"))
    gleam.String -> #(state, doc.from_string("decode.string"))
    gleam.BitArray -> #(state, doc.from_string("decode.bit_array"))
    gleam.Json -> #(state, doc.from_string("decode.string"))
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
      let state = state |> import_module("gleam/list")
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
    gleam.Uuid -> {
      let state = state |> import_module("youid/uuid")
      let doc = call_doc("pgo.text", [call_doc("uuid.to_string", [name])])
      #(state, doc)
    }
    gleam.Json -> {
      let state = state |> import_module("gleam/json")
      let doc = call_doc("pgo.text", [call_doc("json.to_string", [name])])
      #(state, doc)
    }
    gleam.Date -> #(state, call_doc("pgo.date", [name]))
    gleam.Timestamp -> #(state, call_doc("pgo.timestamp", [name]))
    gleam.Int -> #(state, call_doc("pgo.int", [name]))
    gleam.Float -> #(state, call_doc("pgo.float", [name]))
    gleam.Bool -> #(state, call_doc("pgo.bool", [name]))
    gleam.String -> #(state, call_doc("pgo.text", [name]))
    gleam.BitArray -> #(state, call_doc("pgo.bytea", [name]))
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
      let state = state |> import_qualified("gleam/option", "type Option")
      let #(state, inner_type) = gleam_type_to_field_type(state, type_)
      #(state, call_doc("Option", [inner_type]))
    }
    gleam.Uuid -> #(
      state |> import_qualified("youid/uuid", "type Uuid"),
      doc.from_string("Uuid"),
    )
    gleam.Date -> #(state, doc.from_string("#(Int, Int, Int)"))
    gleam.Timestamp -> #(
      state,
      doc.from_string("#(#(Int, Int, Int), #(Int, Int, Int))"),
    )
    gleam.Int -> #(state, doc.from_string("Int"))
    gleam.Float -> #(state, doc.from_string("Float"))
    gleam.Bool -> #(state, doc.from_string("Bool"))
    gleam.String -> #(state, doc.from_string("String"))
    gleam.Json -> #(state, doc.from_string("String"))
    gleam.BitArray -> #(state, doc.from_string("BitArray"))
  }
}

/// Generates the code for a single file containing a bunch of typed queries.
///
pub fn generate_code(queries: List(TypedQuery), version: String) -> String {
  let #(state, queries_docs) = {
    let state = default_codegen_state()
    use #(state, docs), query <- list.fold(over: queries, from: #(state, []))
    let #(state, doc) = query_doc(state, version, query)
    #(state, [doc, ..docs])
  }
  let queries_docs = list.reverse(queries_docs)

  let CodeGenState(
    imports:,
    needs_uuid_decoder:,
    needs_date_decoder:,
    needs_timestamp_decoder:,
  ) = state

  let utils =
    []
    |> prepend_if(needs_uuid_decoder, doc.from_string(uuid_decoder))
    |> prepend_if(needs_date_decoder, doc.from_string(date_decoder))
    |> prepend_if(needs_timestamp_decoder, doc.from_string(timestamp_decoder))

  // We always want to output the imports and the code for the queries.
  // But in case we also need some helpers we add a final section to our file
  // with the hard coded helpers we need for the code to compile.
  let code =
    [imports_doc(imports), ..queries_docs]
    |> doc.join(with: doc.lines(2))

  case utils {
    [] -> code
    [_, ..] -> {
      [
        code,
        doc.lines(2),
        doc.from_string(string.pad_right("// --- UTILS ", to: 80, with: "-")),
        doc.lines(2),
        doc.join(utils, with: doc.lines(2)),
        doc.line,
      ]
      |> doc.concat
    }
  }
  |> doc.to_string(80)
}

fn imports_doc(imports: Dict(String, Set(String))) -> Document {
  let sorted_imports =
    dict.to_list(imports)
    |> list.sort(fn(one, other) { string.compare(one.0, other.0) })

  {
    use #(module, imported_values) <- list.map(sorted_imports)
    let import_line = doc.from_string("import " <> module)
    use <- bool.guard(when: set.is_empty(imported_values), return: import_line)

    let imported_values =
      set.to_list(imported_values)
      |> list.sort(string.compare)
      |> list.map(doc.from_string)
      |> doc.join(with: doc.break(", ", ","))
      |> doc.group

    [
      import_line,
      doc.from_string(".{"),
      [doc.soft_break, imported_values]
        |> doc.concat
        |> doc.group
        |> doc.nest(by: indent),
      doc.soft_break,
      doc.from_string("}"),
    ]
    |> doc.concat
  }
  |> doc.join(with: doc.line)
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
    name:,
    content:,
    comment: _,
    params:,
    returns:,
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
  let TypedQuery(comment:, name:, file:, ..) = query
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
  let TypedQuery(name:, returns:, file:, ..) = query
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

const uuid_decoder = "/// A decoder to decode `Uuid`s coming from a Postgres query.
///
fn uuid_decoder() {
  decode.then(decode.bit_array, fn(uuid) {
    case uuid.from_bit_array(uuid) {
      Ok(uuid) -> decode.into(uuid)
      Error(_) -> decode.fail(\"uuid\")
    }
  })
}"

const date_decoder = "/// A decoder to decode `date`s coming from a Postgres query.
///
fn date_decoder() {
  use dynamic <- decode.then(decode.dynamic)
  case pgo.decode_date(dynamic) {
    Ok(date) -> decode.into(date)
    Error(_) -> decode.fail(\"date\")
  }
}"

const timestamp_decoder = "/// A decoder to decode `timestamp`s coming from a Postgres query.
///
fn timestamp_decoder() {
  use dynamic <- decode.then(decode.dynamic)
  case pgo.decode_timestamp(dynamic) {
    Ok(timestamp) -> decode.into(timestamp)
    Error(_) -> decode.fail(\"timestamp\")
  }
}"

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

// --- UTILS TO WORK WITH STATE ------------------------------------------------

fn import_module(state: CodeGenState, name: String) -> CodeGenState {
  let imports = case dict.has_key(state.imports, name) {
    False -> dict.insert(state.imports, name, set.new())
    True -> state.imports
  }
  CodeGenState(..state, imports:)
}

fn import_qualified(
  state: CodeGenState,
  module: String,
  imported: String,
) -> CodeGenState {
  let imports =
    dict.upsert(state.imports, module, fn(imported_values) {
      case imported_values {
        Some(imported_values) -> set.insert(imported_values, imported)
        None -> set.from_list([imported])
      }
    })

  CodeGenState(..state, imports:)
}

// --- MISC UTILS --------------------------------------------------------------

fn prepend_if(list: List(a), condition: Bool, item: a) -> List(a) {
  case condition {
    True -> [item, ..list]
    False -> list
  }
}
