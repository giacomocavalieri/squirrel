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
import justin
import non_empty_list.{type NonEmptyList}
import simplifile
import splitter.{type Splitter}
import squirrel/internal/error.{
  type Error, CannotReadFile, QueryFileHasInvalidName,
  QueryReturnsMultipleValuesWithTheSameName,
}
import squirrel/internal/gleam.{type EnumVariant, type TypeIdentifier}
import tote/bag

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
    params: List(Parameter),
    returns: List(gleam.Field),
  )
}

pub type Parameter {
  Parameter(
    /// The name inferred for this parameter.
    name: Option(gleam.ValueIdentifier),
    type_: gleam.Type,
  )
}

/// Turns an untyped query into a typed one.
///
pub fn add_types(
  to query: UntypedQuery,
  params params: List(gleam.Type),
  returns returns: List(gleam.Field),
) -> Result(TypedQuery, Error) {
  let UntypedQuery(file:, name:, comment:, content:, starting_line:) = query
  let params = infer_parameter_names(query, params)
  case duplicate_names(returns) {
    [] ->
      Ok(TypedQuery(
        file:,
        name:,
        comment:,
        content:,
        starting_line:,
        params:,
        returns:,
      ))

    names ->
      Error(QueryReturnsMultipleValuesWithTheSameName(
        file:,
        content:,
        starting_line:,
        names:,
      ))
  }
}

fn duplicate_names(fields: List(gleam.Field)) -> List(String) {
  let names = {
    use bag, gleam.Field(label:, ..) <- list.fold(fields, from: bag.new())
    bag.insert(bag, 1, of: gleam.value_identifier_to_string(label))
  }

  use duplicate_names, field, copies <- bag.fold(names, from: [])
  case copies {
    1 -> duplicate_names
    _ -> [field, ..duplicate_names]
  }
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
    gleam.value_identifier(file_name)
    |> result.map_error(QueryFileHasInvalidName(
      file:,
      reason: _,
      suggested_name: gleam.similar_value_identifier_string(file_name)
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
  case string.trim_start(query) {
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
    // All the enums used in the module, this maps from name of the enum to a
    // list of its variants and what kind of helpers need to be generated for
    // the enum encoding/decoding.
    enums: Dict(TypeIdentifier, EnumCodeGenData),
  )
}

/// Data needed to perform codegen for an enum.
///
type EnumCodeGenData {
  EnumCodeGenData(
    /// Needed to know what kind of functions to generate for the specific case.
    ///
    required_helpers: RequiredHelpers,
    /// The original name used to define the enum in postgres to generate a
    /// useful comment.
    ///
    original_name: String,
    /// The variants of the enum.
    ///
    variants: NonEmptyList(EnumVariant),
  )
}

type RequiredHelpers {
  NeedsEncoderAndDecoder
  NeedsDecoder
  NeedsEncoder
  NoHelpers
}

fn merge_helpers(
  one: RequiredHelpers,
  other: RequiredHelpers,
) -> RequiredHelpers {
  case one, other {
    NoHelpers, other | other, NoHelpers -> other
    NeedsEncoderAndDecoder, _ | _, NeedsEncoderAndDecoder ->
      NeedsEncoderAndDecoder
    NeedsDecoder, NeedsEncoder | NeedsEncoder, NeedsDecoder ->
      NeedsEncoderAndDecoder
    NeedsEncoder, NeedsEncoder -> NeedsEncoder
    NeedsDecoder, NeedsDecoder -> NeedsDecoder
  }
}

fn default_codegen_state() {
  CodeGenState(
    imports: dict.new(),
    needs_uuid_decoder: False,
    enums: dict.new(),
  )
  |> import_module("gleam/dynamic/decode")
  |> import_module("pog")
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
    gleam.Date -> #(state, doc.from_string("pog.calendar_date_decoder()"))
    gleam.TimeOfDay -> {
      #(state, doc.from_string("pog.calendar_time_of_day_decoder()"))
    }
    gleam.Timestamp -> #(state, doc.from_string("pog.timestamp_decoder()"))
    gleam.Int -> #(state, doc.from_string("decode.int"))
    gleam.Float -> #(state, doc.from_string("decode.float"))
    gleam.Numeric -> #(state, doc.from_string("pog.numeric_decoder()"))
    gleam.Bool -> #(state, doc.from_string("decode.bool"))
    gleam.String -> #(state, doc.from_string("decode.string"))
    gleam.BitArray -> #(state, doc.from_string("decode.bit_array"))
    gleam.Json -> #(state, doc.from_string("decode.string"))
    gleam.Enum(original_name:, name: enum_name, variants:) -> #(
      add_enum_helpers(state, original_name, enum_name, variants, NeedsDecoder),
      doc.from_string(enum_decoder_name(enum_name) <> "()"),
    )
  }
}

fn enum_decoder_name(enum_name: TypeIdentifier) -> String {
  gleam.type_identifier_to_value_identifier(enum_name)
  |> gleam.value_identifier_to_string
  |> string.append("_decoder")
}

fn gleam_type_to_encoder(
  state: CodeGenState,
  type_: gleam.Type,
  name: String,
) -> #(CodeGenState, Document) {
  let name = doc.from_string(name)
  case type_ {
    gleam.List(type_) -> {
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let map_fn = fn_doc(["value"], inner_encoder)
      let doc = call_doc("pog.array", [map_fn, name])
      #(state, doc)
    }
    gleam.Option(type_) -> {
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let doc =
        call_doc("pog.nullable", [fn_doc(["value"], inner_encoder), name])
      #(state, doc)
    }
    gleam.Uuid -> {
      let state = state |> import_module("youid/uuid")
      let doc = call_doc("pog.text", [call_doc("uuid.to_string", [name])])
      #(state, doc)
    }
    gleam.Json -> {
      let state = state |> import_module("gleam/json")
      let doc = call_doc("pog.text", [call_doc("json.to_string", [name])])
      #(state, doc)
    }
    gleam.Date -> #(state, call_doc("pog.calendar_date", [name]))
    gleam.TimeOfDay -> #(state, call_doc("pog.calendar_time_of_day", [name]))
    gleam.Timestamp -> #(state, call_doc("pog.timestamp", [name]))
    gleam.Int -> #(state, call_doc("pog.int", [name]))
    gleam.Float | gleam.Numeric -> #(state, call_doc("pog.float", [name]))
    gleam.Bool -> #(state, call_doc("pog.bool", [name]))
    gleam.String -> #(state, call_doc("pog.text", [name]))
    gleam.BitArray -> #(state, call_doc("pog.bytea", [name]))
    gleam.Enum(original_name:, name: enum_name, variants:) -> #(
      add_enum_helpers(state, original_name, enum_name, variants, NeedsEncoder),
      call_doc(enum_encoder_name(enum_name), [name]),
    )
  }
}

fn enum_encoder_name(enum_name: TypeIdentifier) -> String {
  gleam.type_identifier_to_value_identifier(enum_name)
  |> gleam.value_identifier_to_string
  |> string.append("_encoder")
}

type TypePosition {
  EnumField
  FunctionArgument
}

fn gleam_type_to_field_type(
  state: CodeGenState,
  type_: gleam.Type,
  position: TypePosition,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.List(type_) -> {
      let #(state, inner_type) =
        gleam_type_to_field_type(state, type_, position)
      #(state, call_doc("List", [inner_type]))
    }
    gleam.Option(type_) -> {
      let state = state |> import_qualified("gleam/option", "type Option")
      let #(state, inner_type) =
        gleam_type_to_field_type(state, type_, position)
      #(state, call_doc("Option", [inner_type]))
    }
    gleam.Uuid -> #(
      state |> import_qualified("youid/uuid", "type Uuid"),
      doc.from_string("Uuid"),
    )
    gleam.Date -> {
      let state = state |> import_qualified("gleam/time/calendar", "type Date")
      #(state, doc.from_string("Date"))
    }
    gleam.TimeOfDay -> {
      let state =
        state |> import_qualified("gleam/time/calendar", "type TimeOfDay")
      #(state, doc.from_string("TimeOfDay"))
    }
    gleam.Timestamp -> {
      let state =
        state |> import_qualified("gleam/time/timestamp", "type Timestamp")
      #(state, doc.from_string("Timestamp"))
    }
    gleam.Int -> #(state, doc.from_string("Int"))
    gleam.Float | gleam.Numeric -> #(state, doc.from_string("Float"))
    gleam.Bool -> #(state, doc.from_string("Bool"))
    gleam.String -> #(state, doc.from_string("String"))
    gleam.Json ->
      case position {
        EnumField -> #(state, doc.from_string("String"))
        FunctionArgument -> {
          let state = state |> import_qualified("gleam/json", "type Json")
          #(state, doc.from_string("Json"))
        }
      }
    gleam.BitArray -> #(state, doc.from_string("BitArray"))
    gleam.Enum(original_name:, name:, variants:) -> #(
      add_enum_helpers(state, original_name, name, variants, NoHelpers),
      gleam.type_identifier_to_string(name) |> doc.from_string,
    )
  }
}

/// Generates the code for a single file containing a bunch of typed queries.
///
pub fn generate_code(
  version version: String,
  // The directory all the queries come from.
  for queries: List(TypedQuery),
  from directory: String,
) -> String {
  // We need to sort the queries before generating any code, otherwise the order
  // with which they will appear in the generated file won't be reproducible!
  // That could cause CI checks like `gleam run -m squirrel check` to fail.
  //
  let queries =
    list.sort(queries, fn(one, other) { string.compare(one.file, other.file) })

  let #(state, queries_docs) = {
    let state = default_codegen_state()
    use #(state, docs), query <- list.fold(over: queries, from: #(state, []))
    let #(state, doc) = query_doc(state, version, query)
    #(state, [doc, ..docs])
  }
  let queries_docs = list.reverse(queries_docs)

  let CodeGenState(imports:, needs_uuid_decoder:, enums:) = state

  let utils = case needs_uuid_decoder {
    True -> [doc.from_string(uuid_decoder)]
    False -> []
  }

  // We always want to output the imports and the code for the queries.
  // But in case we also need some helpers we add a final section to our file
  // with the hard coded helpers we need for the code to compile.
  let code =
    [
      imports_doc(imports),
      doc.lines(2),
      doc.join(queries_docs, with: doc.lines(2)),
    ]
    |> doc.concat

  let code = case dict.is_empty(enums) {
    True -> code
    False ->
      [
        code,
        doc.lines(2),
        separator_comment("Enums"),
        doc.lines(2),
        enums_doc(version, enums),
      ]
      |> doc.concat
  }

  let code = case utils {
    [] -> code
    [_, ..] -> {
      [
        code,
        doc.lines(2),
        separator_comment("Encoding/decoding utils"),
        doc.lines(2),
        doc.join(utils, with: doc.lines(2)),
      ]
      |> doc.concat
    }
  }

  doc.concat([
    doc.from_string(module_doc(version, directory)),
    doc.lines(2),
    code,
    doc.line,
  ])
  |> doc.to_string(80)
}

fn separator_comment(value: String) -> Document {
  string.pad_end("// --- " <> value <> " ", to: 80, with: "-")
  |> doc.from_string
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

  let constructor_name =
    gleam.value_identifier_to_type_identifier(name)
    |> gleam.type_identifier_to_string
    |> string.append("Row")

  let record_result = record_doc(state, version, constructor_name, query)
  let #(state, record) = case record_result {
    Ok(#(state, record)) -> #(state, doc.append(record, doc.lines(2)))
    Error(_) -> #(state, doc.empty)
  }

  let #(state, decoder) = decoder_doc(state, constructor_name, returns)
  let #(state, _, args, encoders) = {
    // "decoder" and "db" are two arguments that are always defined in the
    // generated function, so we always have them as already used!
    let used_names = set.from_list(["decoder", "db"])
    use acc, param, i <- list.index_fold(params, #(state, used_names, [], []))
    let #(state, used_names, args, encoders) = acc

    let arg = generate_argument_name(used_names, param, i)
    let used_names = set.insert(used_names, arg)
    let #(state, arg_type) =
      gleam_type_to_field_type(state, param.type_, FunctionArgument)
    let #(state, encoder) = gleam_type_to_encoder(state, param.type_, arg)

    let arg = doc.concat([doc.from_string(arg <> ": "), arg_type])
    #(state, used_names, [arg, ..args], [encoder, ..encoders])
  }

  let encoders = list.reverse(encoders)
  let args = [doc.from_string("db: pog.Connection"), ..list.reverse(args)]

  let returned = case returns {
    [] -> doc.from_string("pog.Returned(Nil)")
    _ -> call_doc("pog.Returned", [doc.from_string(constructor_name)])
  }
  let return = call_doc("Result", [returned, doc.from_string("pog.QueryError")])

  let code =
    doc.concat([
      record,
      doc.from_string(function_doc(version, query)),
      doc.line,
      fun_doc(Public, gleam.value_identifier_to_string(name), args, return, [
        let_var("decoder", decoder) |> doc.append(doc.from_string("\n")),
        string_doc(content)
          |> pipe_call_doc("pog.query", _, [])
          |> pipe_all_encoders(encoders)
          |> pipe_call_doc("pog.returning", _, [doc.from_string("decoder")])
          |> pipe_call_doc("pog.execute", _, [doc.from_string("db")]),
      ]),
    ])

  #(state, code)
}

fn generate_argument_name(
  used_names: Set(String),
  param: Parameter,
  position: Int,
) -> String {
  let name = case param.name {
    Some(name) -> gleam.value_identifier_to_string(name)
    None -> "arg_" <> int.to_string(position + 1)
  }

  // Any name ending with "encoder", "decoder" might not be safe to use as there
  // might be decoders/encoders in scope that would end up being shadowed.
  // The perfect approach would require traversing over the query twice, first
  // to understand which encoder/decoder names it needs in scope and then
  // renaming parameters with that same name to avoid shadowing them.
  // However, that's not something I can see happening frequently (how many
  // queries do something like `$1 = uuid_decoder`?) so I do something simpler:
  // if a name ends with `encoder`, `decoder` we always rename it by adding a
  // safe suffix.
  let name = case
    string.ends_with(name, "encoder") || string.ends_with(name, "decoder")
  {
    True -> rename_to_avoid_shadowing_loop(used_names, name, 1)
    False -> name
  }

  // We don't want to use SQL literals as names, in that case we fall back to
  // calling things as "arg_"
  let name = case name {
    "true" | "false" | "null" -> "arg_" <> int.to_string(position + 1)
    _ -> name
  }

  // Now we want to rename the argument to avoid it shadowing other names that
  // have already been used.
  rename_to_avoid_shadowing(used_names, name)
}

fn rename_to_avoid_shadowing(used_names: Set(String), name: String) -> String {
  case set.contains(used_names, name) {
    False -> name
    True -> rename_to_avoid_shadowing_loop(used_names, name, 1)
  }
}

fn rename_to_avoid_shadowing_loop(
  used_names: Set(String),
  name: String,
  tries: Int,
) -> String {
  let candidate = name <> "_" <> int.to_string(tries)
  case set.contains(used_names, candidate) {
    False -> candidate
    True -> rename_to_avoid_shadowing_loop(used_names, name, tries + 1)
  }
}

fn pipe_all_encoders(doc: Document, decoders: List(Document)) -> Document {
  use doc, decoder <- list.fold(over: decoders, from: doc)
  doc |> pipe_call_doc("pog.parameter", _, [decoder])
}

fn module_doc(version: String, directory: String) -> String {
  "//// This module contains the code to run the sql queries defined in
//// `" <> directory <> "`.
//// > 🐿️ This module was generated automatically using " <> version <> " of
//// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
////"
}

fn function_doc(version: String, query: TypedQuery) -> String {
  let TypedQuery(comment:, name:, file:, ..) = query
  let function_name = gleam.value_identifier_to_string(name)

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

  let function_name = gleam.value_identifier_to_string(name)
  let record_doc =
    "/// A row you get from running the `" <> function_name <> "` query
/// defined in `" <> file <> "`.
///
/// > 🐿️ This type definition was generated automatically using " <> version <> " of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"

  let #(state, fields) = {
    use #(state, fields), field <- list.fold(returns, from: #(state, []))
    let label =
      doc.from_string(gleam.value_identifier_to_string(field.label) <> ": ")
    let #(state, field_type) =
      gleam_type_to_field_type(state, field.type_, EnumField)
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

/// Returns the document for the definition and encoding/decoding of all enums
/// in the dictionary.
///
fn enums_doc(
  version: String,
  enums: Dict(TypeIdentifier, EnumCodeGenData),
) -> Document {
  use doc, name, enum_data <- dict.fold(enums, doc.empty)
  doc.append(doc, enum_doc(version, name, enum_data))
}

/// Returns the document with the enum definition and any additional helper that
/// might be needed to encode and decode it.
///
fn enum_doc(
  version: String,
  enum_name: TypeIdentifier,
  enum_data: EnumCodeGenData,
) -> Document {
  let EnumCodeGenData(original_name:, required_helpers:, variants:) = enum_data

  case required_helpers {
    NeedsDecoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_decoder_doc(enum_name, variants),
    ]
    NeedsEncoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_encoder_doc(enum_name, variants),
    ]
    NeedsEncoderAndDecoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_decoder_doc(enum_name, variants),
      enum_encoder_doc(enum_name, variants),
    ]
    NoHelpers -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
    ]
  }
  |> doc.join(with: doc.lines(2))
}

fn enum_type_definition_doc(
  version: String,
  enum_name: TypeIdentifier,
  original_name: String,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let string_enum_name = gleam.type_identifier_to_string(enum_name)
  let variants =
    non_empty_list.map(variants, fn(variant) {
      gleam.type_identifier_to_string(variant.name)
      |> doc.from_string
    })

  let enum_doc =
    "/// Corresponds to the Postgres `" <> original_name <> "` enum.
///
/// > 🐿️ This type definition was generated automatically using " <> version <> " of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"

  doc.concat([
    doc.from_string(enum_doc),
    doc.line,
    doc.from_string("pub type " <> string_enum_name <> " "),
    block(non_empty_list.to_list(variants)),
  ])
}

fn enum_encoder_doc(
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let case_lines = {
    use variant <- non_empty_list.map(variants)
    [
      doc.from_string(gleam.type_identifier_to_string(variant.name)),
      doc.from_string(" -> "),
      string_doc(variant.string_representation),
    ]
    |> doc.concat
  }

  let var_name =
    name
    |> gleam.type_identifier_to_value_identifier
    |> gleam.value_identifier_to_string

  let case_ =
    doc.concat([
      doc.from_string("case " <> var_name <> " "),
      block(non_empty_list.to_list(case_lines)),
    ])

  let case_ = pipe_call_doc("pog.text", case_, [])

  fun_doc(
    Private,
    enum_encoder_name(name),
    [doc.from_string(var_name)],
    doc.from_string("pog.Value"),
    [case_],
  )
}

fn enum_decoder_doc(
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let success_case_lines = {
    use variant <- non_empty_list.map(variants)
    doc.concat([
      string_doc(variant.string_representation),
      doc.from_string(" -> "),
      call_doc("decode.success", [
        gleam.type_identifier_to_string(variant.name)
        |> doc.from_string,
      ]),
    ])
  }

  let failure_case_line =
    [
      doc.from_string("_ -> "),
      call_doc("decode.failure", [
        doc.from_string(gleam.type_identifier_to_string(variants.first.name)),
        string_doc(gleam.type_identifier_to_string(name)),
      ]),
    ]
    |> doc.concat

  let var_name =
    name
    |> gleam.type_identifier_to_value_identifier
    |> gleam.value_identifier_to_string

  let case_ =
    doc.concat([
      doc.from_string("case " <> var_name <> " "),
      success_case_lines
        |> non_empty_list.to_list
        |> list.append([failure_case_line])
        |> block,
    ])

  let enum_decoder_type =
    doc.from_string(
      "decode.Decoder(" <> gleam.type_identifier_to_string(name) <> ")",
    )

  fun_doc(Private, enum_decoder_name(name), [], enum_decoder_type, [
    doc.from_string("use " <> var_name <> " <- decode.then(decode.string)"),
    case_,
  ])
}

const uuid_decoder = "/// A decoder to decode `Uuid`s coming from a Postgres query.
///
fn uuid_decoder() {
  use bit_array <- decode.then(decode.bit_array)
  case uuid.from_bit_array(bit_array) {
    Ok(uuid) -> decode.success(uuid)
    Error(_) -> decode.failure(uuid.v7(), \"Uuid\")
  }
}"

/// A decoder that discards its value and always returns `Nil` instead.
///
const nil_decoder = "decode.map(decode.dynamic, fn(_) { Nil })"

/// A pretty printed decoder that decodes an n-item dynamic tuple with the given
/// constructor wrapping the returned rows from a query.
///
/// If the query returns no columns (that is `returns == []`), then we default
/// to building decoder that always returns `Nil`.
///
fn decoder_doc(
  state: CodeGenState,
  constructor: String,
  returns: List(gleam.Field),
) -> #(CodeGenState, Document) {
  let fallback = #(state, doc.from_string(nil_decoder))
  use <- bool.guard(when: returns == [], return: fallback)

  let #(state, parameters, labelled_names) = {
    use acc, field, i <- list.index_fold(returns, #(state, [], []))
    let #(state, parameters, labelled_names) = acc

    let label = gleam.value_identifier_to_string(field.label)
    let labelled_names = [doc.from_string(label <> ":"), ..labelled_names]

    let position = int.to_string(i) |> doc.from_string
    let #(state, decoder) = gleam_type_to_decoder(state, field.type_)
    let param =
      doc.from_string("use " <> label <> " <- ")
      |> doc.append(call_doc("decode.field", [position, decoder]))
    let parameters = [param, ..parameters]

    #(state, parameters, labelled_names)
  }
  let parameters = list.reverse(parameters)
  let labelled_names = list.reverse(labelled_names)

  let success_line =
    nested_calls_doc("decode.success", constructor, labelled_names)

  let doc = block(list.append(parameters, [success_line]))
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
  let function = case rest {
    [] -> doc.from_string("|> " <> function)
    [_, ..] -> call_doc("|> " <> function, rest)
  }

  [first, doc.line, function]
  |> doc.concat
}

/// A pretty printed function call.
///
fn call_doc(function: String, args: List(Document)) -> Document {
  [doc.from_string(function), comma_list("(", args, ")") |> doc.group]
  |> doc.concat
  |> doc.group
}

/// This is a special case of a call document. To accomodate for a special rule
/// of the Gleam formatter: when we have a function call that has a single other
/// function as its one and only argument.
///
/// ```gleam
/// first(second(arg_1, arg_2, arg_3, ..., arg_n))
/// ```
///
/// When this needs to be broken, the formatter will only split the arguments of
/// the second call like this:
///
/// ```gleam
/// first(second(
///   arg_1,
///   ...,
///   arg_n
/// ))
/// ```
///
/// Given the first and second function, and the arguments of the second
/// function, this function builds a document that behaves like that.
///
fn nested_calls_doc(
  first: String,
  second: String,
  arguments: List(Document),
) -> Document {
  [
    doc.from_string(first),
    doc.from_string("("),
    // ^^ For the first call we don't add any breakable space after the `(`, so
    //    that the only thing that can get broken on multiple lines are the
    //    arguments of the second function
    call_doc(second, arguments),
    // ^^ And the second call is broken and behaves as usual, with its arguments
    //    being nested
    doc.from_string(")"),
  ]
  |> doc.concat
}

/// A pretty printed Gleam block.
///
fn block(body: List(Document)) -> Document {
  [
    doc.from_string("{"),
    doc.line |> doc.nest(by: indent),
    body
      |> doc.join(with: doc.line)
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
}

type Publicity {
  Public
  Private
}

/// A pretty printed public function definition.
///
fn fun_doc(
  publicity: Publicity,
  name: String,
  args: List(Document),
  return_type: Document,
  body: List(Document),
) -> Document {
  let publicity = case publicity {
    Private -> ""
    Public -> "pub "
  }

  [
    [
      doc.from_string(publicity <> "fn " <> name),
      comma_list("(", args, ")"),
      doc.from_string(" -> "),
      return_type,
      doc.from_string(" "),
    ]
      |> doc.concat
      |> doc.group,
    block(body),
  ]
  |> doc.concat
  |> doc.group
}

fn fn_doc(args: List(String), body: Document) -> Document {
  [
    doc.from_string("fn"),
    comma_list("(", list.map(args, doc.from_string), ") {")
      |> doc.group,
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
fn let_var(name: String, body: Document) -> Document {
  [doc.from_string("let " <> name <> " ="), doc.space, body]
  |> doc.concat
  |> doc.group
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

/// A comma separated list of items with some given open and closed delimiters.
///
fn comma_list(
  open: String,
  content: List(Document),
  close: String,
) -> Document {
  case content {
    [] -> doc.from_string(open <> close)
    _ ->
      [
        doc.from_string(open),
        [
          // We want the first break to be nested
          // in case the group is broken.
          doc.soft_break,
          doc.join(content, doc.break(", ", ",")),
        ]
          |> doc.concat
          |> doc.nest(by: indent),
        doc.break("", ","),
        doc.from_string(close),
      ]
      |> doc.concat
  }
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

fn add_enum_helpers(
  state: CodeGenState,
  original_name: String,
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
  required_helpers: RequiredHelpers,
) -> CodeGenState {
  CodeGenState(..state, enums: {
    use value <- dict.upsert(state.enums, name)
    case value {
      None -> EnumCodeGenData(required_helpers:, variants:, original_name:)
      Some(EnumCodeGenData(required_helpers: helpers, ..) as data) -> {
        let required_helpers = merge_helpers(required_helpers, helpers)
        EnumCodeGenData(..data, required_helpers:)
      }
    }
  })
}

// --- SQL QUERY PARAMETERS ANALYSIS -------------------------------------------
// This code analyses SQL queries to try and understand the names of the query
// parameters used in each.

type Context {
  NoIdentifier

  // We've found `$1 ...`
  QueryParameter(Int)
  // We've found `$1 = ...`
  QueryParameterEquality(Int)

  // We've found `wibble ...`
  Identifier(Name)
  // We've found `wibble = ...`
  IdentifierEquality(Name)
}

type Name {
  Column(String)
  TableAndColumn(String, String)
}

type Splitters {
  Splitters(whitespace: Splitter, comments: Splitter, strings: Splitter)
}

fn infer_parameter_names(
  query: UntypedQuery,
  params: List(gleam.Type),
) -> List(Parameter) {
  let names = parameter_names(query.content)
  list.index_map(params, fn(type_, i) {
    case dict.get(names, i + 1) {
      Ok(name) -> Parameter(name: Some(name), type_:)
      Error(_) -> Parameter(name: None, type_:)
    }
  })
}

fn parameter_names(query: String) -> Dict(Int, gleam.ValueIdentifier) {
  let query = string.lowercase(query)
  let splitters =
    Splitters(
      whitespace: splitter.new([" ", "\n", "\t", "\r"]),
      comments: splitter.new(["/*", "*/"]),
      strings: splitter.new(["''", "\\'", "'"]),
    )

  parameter_names_loop(splitters, query, NoIdentifier, dict.new())
  |> dict.fold(dict.new(), fn(acc, parameter, names) {
    case pick_name(names) {
      Ok(name) -> dict.insert(acc, parameter, name)
      Error(_) -> acc
    }
  })
}

fn pick_name(names: List(Name)) -> Result(gleam.ValueIdentifier, Nil) {
  list.find_map(list.reverse(names), fn(name) {
    let name = case name {
      Column(name) -> justin.snake_case(name)
      TableAndColumn(table, column) -> {
        let column = string.remove_prefix(table, from: column)
        justin.snake_case(table) <> "_" <> justin.snake_case(column)
      }
    }
    gleam.value_identifier(name)
  })
}

fn add_name(
  names: Dict(a, List(b)),
  parameter: a,
  name: b,
) -> Dict(a, List(b)) {
  dict.upsert(names, parameter, fn(existing) {
    case existing {
      Some(existing) -> [name, ..existing]
      None -> [name]
    }
  })
}

fn parameter_names_loop(
  splitters: Splitters,
  query: String,
  context: Context,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case query {
    // COMMENTS
    // We strip those out totally ignoring their content.
    // A comment also won't change the current context.
    "/*" <> query -> multiline_comment_loop(splitters, query, context, 1, names)
    "--" <> query ->
      case string.split_once(query, on: "\n") {
        Error(_) -> names
        Ok(#(_comment, query)) ->
          parameter_names_loop(splitters, query, context, names)
      }

    // STRINGS
    // We strip those out ignoring their content.
    "'" <> query -> string_loop(splitters, query, names)

    // WHITESPACE
    // Whitespace is ignored and we just move forward.
    // It also doesn't change the current context.
    " " <> query | "\n" <> query | "\r" <> query | "\t" <> query ->
      parameter_names_loop(splitters, query, context, names)

    // QUOTED IDENTIFIERS
    // Any sequence of characters in between double quotes.
    "\"" <> query ->
      case string.split_once(query, "\"") {
        // The identifier is not closed, this is a syntax error, we just return
        // all the names we've figured out so far
        Error(_) -> names
        // This could be a dotted identifier!!
        Ok(#(identifier, "." <> query)) ->
          dotted_identifier(splitters, query, context, identifier, names)
        // We've found an identifier, and so update the names and context
        // accordingly.
        Ok(#(identifier, query)) ->
          add_identifier(splitters, query, Column(identifier), context, names)
      }

    // IDENTIFIERS/KEYWORDS
    // If we see the start of an identifier we're gonna have to parse the whole
    // word and then keep going to see if it is the name of a query parameter.
    "a" as letter <> query
    | "b" as letter <> query
    | "c" as letter <> query
    | "d" as letter <> query
    | "e" as letter <> query
    | "f" as letter <> query
    | "g" as letter <> query
    | "h" as letter <> query
    | "i" as letter <> query
    | "j" as letter <> query
    | "k" as letter <> query
    | "l" as letter <> query
    | "m" as letter <> query
    | "n" as letter <> query
    | "o" as letter <> query
    | "p" as letter <> query
    | "q" as letter <> query
    | "r" as letter <> query
    | "s" as letter <> query
    | "t" as letter <> query
    | "u" as letter <> query
    | "v" as letter <> query
    | "w" as letter <> query
    | "x" as letter <> query
    | "y" as letter <> query
    | "z" as letter <> query
    | "_" as letter <> query ->
      identifier_loop(splitters, query, letter, context, names)

    // QUERY PARAMETERS
    "$1" <> query -> parameter_loop(splitters, query, context, 1, names)
    "$2" <> query -> parameter_loop(splitters, query, context, 2, names)
    "$3" <> query -> parameter_loop(splitters, query, context, 3, names)
    "$4" <> query -> parameter_loop(splitters, query, context, 4, names)
    "$5" <> query -> parameter_loop(splitters, query, context, 5, names)
    "$6" <> query -> parameter_loop(splitters, query, context, 6, names)
    "$7" <> query -> parameter_loop(splitters, query, context, 7, names)
    "$8" <> query -> parameter_loop(splitters, query, context, 8, names)
    "$9" <> query -> parameter_loop(splitters, query, context, 9, names)

    // EQUALITY OPERATOR
    // If we see a check for equality we need to update the current state
    // accordingly.
    "=" <> query ->
      case context {
        NoIdentifier | QueryParameterEquality(_) | IdentifierEquality(_) ->
          recover_from_error(splitters, query, names)
        QueryParameter(parameter) ->
          parameter_names_loop(
            splitters,
            query,
            QueryParameterEquality(parameter),
            names,
          )
        Identifier(identifier) ->
          parameter_names_loop(
            splitters,
            query,
            IdentifierEquality(identifier),
            names,
          )
      }

    // EMPTY STRING
    // That means we're done, there's nothing left to do and we can return all
    // the names we've figured out.
    "" -> names

    // UNEXPECTED CHARACTER
    // If we see anything strange that doesn't really look like an identifier
    // or a keyword we will just ignore them and jump forward to the end of the
    // word/operator/number/unexpected character.
    _ -> recover_from_error(splitters, query, names)
  }
}

fn parameter_loop(
  splitters: Splitters,
  query: String,
  context: Context,
  number: Int,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case query {
    "0" <> query ->
      parameter_loop(splitters, query, context, number * 10, names)
    "1" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 1, names)
    "2" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 2, names)
    "3" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 3, names)
    "4" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 4, names)
    "5" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 5, names)
    "6" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 6, names)
    "7" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 7, names)
    "8" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 8, names)
    "9" <> query ->
      parameter_loop(splitters, query, context, number * 10 + 9, names)

    // We've found a whitespace, so we know the query parameter is over with, we
    // can now check the context and see if we can record any name for it.
    "" | " " <> _ | "\n" <> _ | "\r" <> _ | "\t" <> _ ->
      add_parameter(splitters, query, number, context, names)

    // We've found a strange character meaning that this is not actually a query
    // parameter, but most likely a syntax error. We just bail out and go back
    // to the main loop with a new empty context to avoid this error propagating
    // further and having us infer invalid names.
    _ -> recover_from_error(splitters, query, names)
  }
}

fn string_loop(
  splitters: Splitters,
  query: String,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case splitter.split(splitters.strings, query) {
    // The string is over
    #(_, "'", query) ->
      parameter_names_loop(splitters, query, NoIdentifier, names)

    // We've found the escaped closing character, that means we're still inside
    // the string and have to keep going.
    #(_, "''", query) | #(_, "\\'", query) ->
      string_loop(splitters, query, names)

    // This is a string that is not closed, we just return the names we've
    // figured out so far
    #(_, _, _) -> names
  }
}

fn multiline_comment_loop(
  splitters: Splitters,
  query: String,
  context: Context,
  expected_closing_sequences: Int,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case expected_closing_sequences {
    0 -> parameter_names_loop(splitters, query, context, names)
    _ ->
      case splitter.split(splitters.comments, query) {
        #(_, "*/", query) ->
          multiline_comment_loop(
            splitters,
            query,
            context,
            expected_closing_sequences - 1,
            names,
          )
        #(_, "/*", query) ->
          multiline_comment_loop(
            splitters,
            query,
            context,
            expected_closing_sequences + 1,
            names,
          )
        // This means the comments are unbalanced, so the query is not right.
        // We just return the names we've found so far!
        _ -> names
      }
  }
}

fn identifier_loop(
  splitters: Splitters,
  query: String,
  identifier: String,
  context: Context,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case query {
    "a" as letter <> query
    | "b" as letter <> query
    | "c" as letter <> query
    | "d" as letter <> query
    | "e" as letter <> query
    | "f" as letter <> query
    | "g" as letter <> query
    | "h" as letter <> query
    | "i" as letter <> query
    | "j" as letter <> query
    | "k" as letter <> query
    | "l" as letter <> query
    | "m" as letter <> query
    | "n" as letter <> query
    | "o" as letter <> query
    | "p" as letter <> query
    | "q" as letter <> query
    | "r" as letter <> query
    | "s" as letter <> query
    | "t" as letter <> query
    | "u" as letter <> query
    | "v" as letter <> query
    | "w" as letter <> query
    | "x" as letter <> query
    | "y" as letter <> query
    | "z" as letter <> query
    | "0" as letter <> query
    | "1" as letter <> query
    | "2" as letter <> query
    | "3" as letter <> query
    | "4" as letter <> query
    | "5" as letter <> query
    | "6" as letter <> query
    | "7" as letter <> query
    | "8" as letter <> query
    | "9" as letter <> query
    | "_" as letter <> query ->
      identifier_loop(splitters, query, identifier <> letter, context, names)

    "." <> query ->
      dotted_identifier(splitters, query, context, identifier, names)

    // The identifier is over, we can update the context accordingly!
    _ -> add_identifier(splitters, query, Column(identifier), context, names)
  }
}

fn dotted_identifier(
  splitters: Splitters,
  query: String,
  context: Context,
  table: String,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case query {
    // This is a quoted dotted identifier: `user."table"`
    "\"" <> query ->
      case string.split_once(query, "\"") {
        // The identifier is not closed, this is a syntax error, we just return
        // all the names we've figured out so far
        Error(_) -> names
        // We've found an identifier, and so update the names and context
        // accordingly.
        Ok(#(column, query)) -> {
          let name = TableAndColumn(table, column)
          add_identifier(splitters, query, name, context, names)
        }
      }

    // Otherwise we treat it as a regular dotted identifier.
    _ -> dotted_identifier_loop(splitters, query, context, table, "", names)
  }
}

fn dotted_identifier_loop(
  splitters: Splitters,
  query: String,
  context: Context,
  table: String,
  column: String,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case query {
    "a" as letter <> query
    | "b" as letter <> query
    | "c" as letter <> query
    | "d" as letter <> query
    | "e" as letter <> query
    | "f" as letter <> query
    | "g" as letter <> query
    | "h" as letter <> query
    | "i" as letter <> query
    | "j" as letter <> query
    | "k" as letter <> query
    | "l" as letter <> query
    | "m" as letter <> query
    | "n" as letter <> query
    | "o" as letter <> query
    | "p" as letter <> query
    | "q" as letter <> query
    | "r" as letter <> query
    | "s" as letter <> query
    | "t" as letter <> query
    | "u" as letter <> query
    | "v" as letter <> query
    | "w" as letter <> query
    | "x" as letter <> query
    | "y" as letter <> query
    | "z" as letter <> query
    | "0" as letter <> query
    | "1" as letter <> query
    | "2" as letter <> query
    | "3" as letter <> query
    | "4" as letter <> query
    | "5" as letter <> query
    | "6" as letter <> query
    | "7" as letter <> query
    | "8" as letter <> query
    | "9" as letter <> query
    | "_" as letter <> query -> {
      let column = column <> letter
      dotted_identifier_loop(splitters, query, context, table, column, names)
    }

    // The identifier is over, we can update the context accordingly!
    _ -> {
      let name = TableAndColumn(table, column)
      add_identifier(splitters, query, name, context, names)
    }
  }
}

fn add_identifier(
  splitters: Splitters,
  query: String,
  identifier: Name,
  context: Context,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case context {
    // We're not comparing the identifier agains a query parameter, so we keep
    // track of this new latest identifier we might be interested in!
    IdentifierEquality(_) | Identifier(_) | NoIdentifier | QueryParameter(_) ->
      parameter_names_loop(splitters, query, Identifier(identifier), names)

    // We've found `$1 = identifier`, that means we have to record the new
    // name and reset the context!
    QueryParameterEquality(number) -> {
      let names = add_name(names, number, identifier)
      parameter_names_loop(splitters, query, NoIdentifier, names)
    }
  }
}

fn add_parameter(
  splitters: Splitters,
  query: String,
  parameter: Int,
  context: Context,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  case context {
    NoIdentifier | Identifier(_) | QueryParameter(_) ->
      parameter_names_loop(splitters, query, QueryParameter(parameter), names)

    // We've found something like `$1 = $2`
    QueryParameterEquality(_) ->
      parameter_names_loop(splitters, query, NoIdentifier, names)

    // We've found something like `wibble = $1`, we have to keep track of
    // this new name we've found for the parameter we've just parsed.
    IdentifierEquality(identifier) -> {
      let names = add_name(names, parameter, identifier)
      parameter_names_loop(splitters, query, NoIdentifier, names)
    }
  }
}

fn recover_from_error(
  splitters: Splitters,
  query: String,
  names: Dict(Int, List(Name)),
) -> Dict(Int, List(Name)) {
  let #(_, _, query) = splitter.split(splitters.whitespace, query)
  parameter_names_loop(splitters, query, NoIdentifier, names)
}
