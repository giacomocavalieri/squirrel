//// Vendored version of https://hex.pm/packages/postgresql_protocol.
//// Ideally this should not be touched unless some really special needs come
//// up.
////
//// I had to make a little change to the existing library:
////  - do not fail with an unexpected Command if the outer message is ok
////  - change the connect function to take in a mug socket instead of asserting
////    and creating one
////
//// This library parses and generates packages for the PostgreSQL Binary Protocol
//// It also provides a basic connection abstraction, but this hasn't been used
//// outside of tests.

import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/result.{try}
import gleam/set
import mug

pub type Connection {
  Connection(socket: mug.Socket, buffer: BitArray, timeout: Int)
}

pub fn connect(host, port, timeout) {
  let options =
    mug.ConnectionOptions(
      host:,
      port:,
      timeout:,
      ip_version_preference: mug.Ipv4Preferred,
    )
  use socket <- result.try(mug.connect(options))
  Ok(Connection(socket:, buffer: <<>>, timeout:))
}

pub type StateInitial {
  StateInitial(parameters: dict.Dict(String, String))
}

pub type State {
  State(
    process_id: Int,
    secret_key: Int,
    parameters: dict.Dict(String, String),
    oids: dict.Dict(Int, fn(BitArray) -> Result(Int, Nil)),
  )
}

fn default_oids() {
  dict.new()
  |> dict.insert(23, fn(col: BitArray) {
    use converted <- try(bit_array.to_string(col))
    int.parse(converted)
  })
}

pub fn start(conn, params) {
  let assert Ok(conn) =
    conn
    |> send(encode_startup_message(params))

  let assert Ok(#(conn, state)) =
    conn
    |> receive_startup_rec(StateInitial(dict.new()))

  #(conn, state)
}

fn receive_startup_rec(conn: Connection, state: StateInitial) {
  case receive(conn) {
    Ok(#(conn, BeAuthenticationOk)) -> receive_startup_rec(conn, state)
    Ok(#(conn, BeParameterStatus(name:, value:))) ->
      receive_startup_rec(
        conn,
        StateInitial(parameters: dict.insert(state.parameters, name, value)),
      )
    Ok(#(conn, BeBackendKeyData(secret_key:, process_id:))) ->
      receive_startup_1(
        conn,
        State(
          parameters: state.parameters,
          secret_key:,
          process_id:,
          oids: default_oids(),
        ),
      )
    Ok(#(_conn, msg)) -> Error(StartupFailedWithUnexpectedMessage(msg))
    Error(err) -> Error(StartupFailedWithError(err))
  }
}

fn receive_startup_1(conn: Connection, state: State) {
  case receive(conn) {
    Ok(#(conn, BeParameterStatus(name:, value:))) ->
      receive_startup_1(
        conn,
        State(..state, parameters: dict.insert(state.parameters, name, value)),
      )
    Ok(#(conn, BeReadyForQuery(_))) -> Ok(#(conn, state))
    Ok(#(_conn, msg)) -> Error(StartupFailedWithUnexpectedMessage(msg))
    Error(err) -> Error(StartupFailedWithError(err))
  }
}

pub type StartupFailed {
  StartupFailedWithUnexpectedMessage(BackendMessage)
  StartupFailedWithError(ReadError)
}

pub type ReadError {
  SocketError(mug.Error)
  ReadDecodeError(MessageDecodingError)
}

pub type MessageDecodingError {
  MessageDecodingError(String)
  MessageIncomplete(BitArray)
  UnknownMessage(data: BitArray)
}

fn dec_err(desc: String, data: BitArray) -> Result(a, MessageDecodingError) {
  Error(MessageDecodingError(desc <> "; data: " <> bit_array.inspect(data)))
}

fn msg_dec_err(desc: String, data: BitArray) -> MessageDecodingError {
  MessageDecodingError(desc <> "; data: " <> bit_array.inspect(data))
}

pub type Command {
  Insert
  Delete
  Update
  Merge
  Select
  Move
  Fetch
  Copy
}

/// Messages originating from the PostgreSQL backend (server)
pub type BackendMessage {
  BeBindComplete
  BeCloseComplete
  BeCommandComplete(Command, Int)
  BeCopyData(data: BitArray)
  BeCopyDone
  BeAuthenticationOk
  BeAuthenticationKerberosV5
  BeAuthenticationCleartextPassword
  BeAuthenticationMD5Password(salt: BitArray)
  BeAuthenticationGSS
  BeAuthenticationGSSContinue(auth_data: BitArray)
  BeAuthenticationSSPI
  BeAuthenticationSASL(mechanisms: List(String))
  BeAuthenticationSASLContinue(data: BitArray)
  BeAuthenticationSASLFinal(data: BitArray)
  BeReadyForQuery(TransactionStatus)
  BeRowDescriptions(List(RowDescriptionField))
  BeMessageDataRow(List(BitArray))
  BeBackendKeyData(process_id: Int, secret_key: Int)
  BeParameterStatus(name: String, value: String)
  BeCopyResponse(
    direction: CopyDirection,
    overall_format: Format,
    codes: List(Format),
  )
  BeNegotiateProtocolVersion(
    newest_minor: Int,
    unrecognized_options: List(String),
  )
  BeNoData
  BeNoticeResponse(set.Set(ErrorOrNoticeField))
  BeNotificationResponse(process_id: Int, channel: String, payload: String)
  BeParameterDescription(List(Int))
  BeParseComplete
  BePortalSuspended
  BeErrorResponse(set.Set(ErrorOrNoticeField))
}

/// Direction of a BeCopyResponse
pub type CopyDirection {
  In
  Out
  Both
}

/// Indicates encoding of column values
pub type Format {
  Text
  Binary
}

/// Lists of parameters can have different encodings.
pub type FormatValue {
  FormatAllText
  FormatAll(Format)
  Formats(List(Format))
}

fn encode_format_value(format) -> BitArray {
  case format {
    FormatAllText -> <<encode_format(Text):16>>
    FormatAll(Text) -> <<1:16, encode_format(Text):16>>
    FormatAll(Binary) -> <<1:16, encode_format(Binary):16>>
    Formats(formats) -> {
      let size = list.length(formats)
      list.fold(formats, <<size:16>>, fn(sum, fmt) {
        <<sum:bits, encode_format(fmt):16>>
      })
    }
  }
}

pub type ParameterValues =
  List(ParameterValue)

pub type ParameterValue {
  Parameter(BitArray)
  Null
}

fn parameters_to_bytes(parameters: ParameterValues) {
  let mapped =
    parameters
    |> list.map(fn(parameter) {
      case parameter {
        Parameter(value) -> <<bit_array.byte_size(value):32, value:bits>>
        Null -> <<-1:32>>
      }
    })

  <<list.length(parameters):16, bit_array.concat(mapped):bits>>
}

pub type FrontendMessage {
  FeBind(
    portal: String,
    statement_name: String,
    parameter_format: FormatValue,
    parameters: ParameterValues,
    result_format: FormatValue,
  )
  FeCancelRequest(process_id: Int, secret_key: Int)
  FeClose(what: What, name: String)
  FeCopyData(data: BitArray)
  FeCopyDone
  FeCopyFail(error: String)
  FeDescribe(what: What, name: String)
  FeExecute(portal: String, return_row_count: Int)
  FeFlush
  FeFunctionCall(
    object_id: Int,
    argument_format: FormatValue,
    arguments: ParameterValues,
    result_format: Format,
  )
  FeGssEncRequest
  FeParse(name: String, query: String, parameter_object_ids: List(Int))
  FeQuery(query: String)
  FeStartupMessage(params: List(#(String, String)))
  FeSslRequest
  FeTerminate
  FeSync
  FeAmbigous(FeAmbigous)
}

// These all share the same message type
pub type FeAmbigous {
  FeGssResponse(data: BitArray)
  FeSaslInitialResponse(name: String, data: BitArray)
  FeSaslResponse(data: BitArray)
  FePasswordMessage(password: String)
}

pub type What {
  PreparedStatement
  Portal
}

fn wire_what(what) {
  case what {
    Portal -> <<"P":utf8>>
    PreparedStatement -> <<"S":utf8>>
  }
}

fn decode_what(binary) {
  case binary {
    <<"P":utf8, rest:bytes>> -> Ok(#(Portal, rest))
    <<"S":utf8, rest:bytes>> -> Ok(#(PreparedStatement, rest))
    _ -> dec_err("only portal and prepared statement are allowed", binary)
  }
}

fn encode_message_data_row(columns) {
  <<list.length(columns):16>>
  |> list.fold(columns, _, fn(sum, column) {
    let len = bit_array.byte_size(column)
    <<sum:bits, len:32, column:bits>>
  })
  |> encode("D", _)
}

fn encode_error_response(fields: set.Set(ErrorOrNoticeField)) -> BitArray {
  fields
  |> set.fold(<<>>, fn(sum, field) { <<sum:bits, encode_field(field):bits>> })
  |> encode("E", _)
}

fn encode_field(field) {
  case field {
    Severity(value) -> <<"S":utf8, value:utf8, 0>>
    SeverityLocalized(value) -> <<"V":utf8, value:utf8, 0>>
    Code(value) -> <<"C":utf8, value:utf8, 0>>
    Message(value) -> <<"M":utf8, value:utf8, 0>>
    Detail(value) -> <<"D":utf8, value:utf8, 0>>
    Hint(value) -> <<"H":utf8, value:utf8, 0>>
    Position(value) -> <<"P":utf8, value:utf8, 0>>
    InternalPosition(value) -> <<"p":utf8, value:utf8, 0>>
    InternalQuery(value) -> <<"q":utf8, value:utf8, 0>>
    Where(value) -> <<"W":utf8, value:utf8, 0>>
    Schema(value) -> <<"s":utf8, value:utf8, 0>>
    Table(value) -> <<"t":utf8, value:utf8, 0>>
    Column(value) -> <<"c":utf8, value:utf8, 0>>
    DataType(value) -> <<"d":utf8, value:utf8, 0>>
    Constraint(value) -> <<"n":utf8, value:utf8, 0>>
    File(value) -> <<"F":utf8, value:utf8, 0>>
    Line(value) -> <<"L":utf8, value:utf8, 0>>
    Routine(value) -> <<"R":utf8, value:utf8, 0>>
    Unknown(key, value) -> <<key:bits, 0, value:utf8, 0>>
  }
}

fn encode_authentication_sasl(mechanisms) -> BitArray {
  list.fold(mechanisms, <<10:32>>, fn(sum, mechanism) {
    <<sum:bits, encode_string(mechanism):bits>>
  })
  |> encode("R", _)
}

fn encode_command_complete(command, rows_num) {
  let rows = int.to_string(rows_num)

  let data =
    case command {
      Insert -> "INSERT 0 " <> rows
      Delete -> "DELETE " <> rows
      Update -> "UPDATE " <> rows
      Merge -> "MERGE " <> rows
      Select -> "SELECT " <> rows
      Move -> "MOVE " <> rows
      Fetch -> "FETCH " <> rows
      Copy -> "COPY " <> rows
    }
    |> encode_string()

  encode("C", data)
}

fn encode_copy_response(
  direction: CopyDirection,
  overall_format: Format,
  codes: List(Format),
) {
  let data = encode_copy_response_rec(overall_format, codes)
  case direction {
    In -> encode("G", data)
    Out -> encode("H", data)
    Both -> encode("W", data)
  }
}

// The format codes to be used for each column. Each must presently be zero
// (text) or one (binary). All must be zero if the overall copy format is
// textual.
fn encode_copy_response_rec(overall_format, codes) {
  case overall_format {
    Text ->
      codes
      |> list.fold(
        <<encode_format(overall_format):8, list.length(codes):16>>,
        fn(sum, _code) { <<sum:bits, encode_format(Text):16>> },
      )
    Binary ->
      codes
      |> list.fold(
        <<encode_format(overall_format):8, list.length(codes):16>>,
        fn(sum, code) { <<sum:bits, encode_format(code):16>> },
      )
  }
}

fn encode_parameter_status(name: String, value: String) -> BitArray {
  encode("S", <<encode_string(name):bits, encode_string(value):bits>>)
}

fn encode_negotiate_protocol_version(
  newest_minor: Int,
  unrecognized_options: List(String),
) {
  list.fold(
    unrecognized_options,
    <<newest_minor:32, list.length(unrecognized_options):32>>,
    fn(sum, option) { <<sum:bits, encode_string(option):bits>> },
  )
  |> encode("v", _)
}

fn encode_notice_response(fields: set.Set(ErrorOrNoticeField)) {
  fields
  |> set.fold(<<>>, fn(sum, field) { <<sum:bits, encode_field(field):bits>> })
  |> encode("N", _)
}

fn encode_notification_response(
  process_id: Int,
  channel: String,
  payload: String,
) {
  encode("A", <<process_id:32, channel:utf8, 0, payload:utf8, 0>>)
}

fn encode_parameter_description(descriptions: List(Int)) {
  descriptions
  |> list.fold(<<list.length(descriptions):16>>, fn(sum, description) {
    <<sum:bits, description:32>>
  })
  |> encode("t", _)
}

fn encode_row_descriptions(fields: List(RowDescriptionField)) {
  fields
  |> list.fold(<<list.length(fields):16>>, fn(sum, field) {
    <<
      sum:bits,
      encode_string(field.name):bits,
      field.table_oid:32,
      field.attr_number:16,
      field.data_type_oid:32,
      field.data_type_size:16,
      field.type_modifier:32,
      field.format_code:16,
    >>
  })
  |> encode("T", _)
}

pub fn encode_backend_message(message: BackendMessage) -> BitArray {
  case message {
    BeMessageDataRow(columns) -> encode_message_data_row(columns)
    BeErrorResponse(fields) -> encode_error_response(fields)
    BeAuthenticationOk -> encode("R", <<0:32>>)
    BeAuthenticationKerberosV5 -> encode("R", <<2:32>>)
    BeAuthenticationCleartextPassword -> encode("R", <<3:32>>)
    BeAuthenticationMD5Password(salt:) -> encode("R", <<5:32, salt:bits>>)
    BeAuthenticationGSS -> encode("R", <<7:32>>)
    BeAuthenticationGSSContinue(data) -> encode("R", <<8:32, data:bits>>)
    BeAuthenticationSSPI -> encode("R", <<9:32>>)
    BeAuthenticationSASL(a) -> encode_authentication_sasl(a)
    BeAuthenticationSASLContinue(data) -> encode("R", <<11:32, data:bits>>)
    BeAuthenticationSASLFinal(data:) -> encode("R", <<12:32, data:bits>>)
    BeBackendKeyData(pid, sk) -> encode("K", <<pid:32, sk:32>>)
    BeBindComplete -> encode("2", <<>>)
    BeCloseComplete -> encode("3", <<>>)
    BeCommandComplete(a, b) -> encode_command_complete(a, b)
    BeCopyData(data) -> encode("d", data)
    BeCopyDone -> encode("c", <<>>)
    BeCopyResponse(a, b, c) -> encode_copy_response(a, b, c)
    BeParameterStatus(name, value) -> encode_parameter_status(name, value)
    BeNegotiateProtocolVersion(a, b) -> encode_negotiate_protocol_version(a, b)
    BeNoData -> encode("n", <<>>)
    BeNoticeResponse(data) -> encode_notice_response(data)
    BeNotificationResponse(a, b, c) -> encode_notification_response(a, b, c)
    BeParameterDescription(a) -> encode_parameter_description(a)
    BeParseComplete -> encode("1", <<>>)
    BePortalSuspended -> encode("s", <<>>)
    BeRowDescriptions(a) -> encode_row_descriptions(a)
    BeReadyForQuery(TransactionStatusIdle) -> encode("Z", <<"I":utf8>>)
    BeReadyForQuery(TransactionStatusInTransaction) -> encode("Z", <<"T":utf8>>)
    BeReadyForQuery(TransactionStatusFailed) -> encode("Z", <<"E":utf8>>)
  }
}

pub fn encode_frontend_message(message: FrontendMessage) {
  case message {
    FeBind(a, b, c, d, e) -> encode_bind(a, b, c, d, e)
    FeCancelRequest(process_id:, secret_key:) -> <<
      16:32,
      1234:16,
      5678:16,
      process_id:32,
      secret_key:32,
    >>
    FeClose(what, name) ->
      encode("C", <<wire_what(what):bits, encode_string(name):bits>>)
    FeCopyData(data) -> encode("d", data)
    FeCopyDone -> <<"c":utf8, 4:32>>
    FeCopyFail(error) -> encode("f", encode_string(error))
    FeDescribe(what, name) ->
      encode("D", <<wire_what(what):bits, encode_string(name):bits>>)
    FeExecute(portal, count) ->
      encode("E", <<encode_string(portal):bits, count:32>>)
    FeFlush -> <<"H":utf8, 4:32>>
    FeFunctionCall(a, b, c, d) -> encode_function_call(a, b, c, d)
    FeGssEncRequest -> <<8:32, 1234:16, 5680:16>>
    FeAmbigous(FeGssResponse(data)) -> encode("p", data)
    FeParse(a, b, c) -> encode_parse(a, b, c)
    FeAmbigous(FePasswordMessage(password)) ->
      encode("p", encode_string(password))
    FeQuery(query) -> encode("Q", encode_string(query))
    FeAmbigous(FeSaslInitialResponse(a, b)) ->
      encode_sasl_initial_response(a, b)
    FeAmbigous(FeSaslResponse(data)) -> encode("p", data)
    FeStartupMessage(params) -> encode_startup_message(params)
    FeSslRequest -> <<8:32, 1234:16, 5679:16>>
    FeSync -> <<"S":utf8, 4:32>>
    FeTerminate -> <<"X":utf8, 4:32>>
  }
}

fn encode(type_char, data) {
  case data {
    <<>> -> <<type_char:utf8, 4:32>>
    _ -> {
      let len = bit_array.byte_size(data) + 4
      <<type_char:utf8, len:32, data:bits>>
    }
  }
}

fn encode_string(str) {
  <<str:utf8, 0>>
}

pub const protocol_version_major = <<3:16>>

pub const protocol_version_minor = <<0:16>>

pub const protocol_version = <<
  protocol_version_major:bits,
  protocol_version_minor:bits,
>>

fn encode_startup_message(params) {
  let packet =
    params
    |> list.fold(<<protocol_version:bits>>, fn(builder, element) {
      let #(key, value) = element
      <<builder:bits, encode_string(key):bits, encode_string(value):bits>>
    })

  let size = bit_array.byte_size(packet) + 5

  <<size:32, packet:bits, 0>>
}

fn encode_sasl_initial_response(name, data) {
  let len = bit_array.byte_size(data)
  encode("p", <<encode_string(name):bits, len:32, data:bits>>)
}

fn encode_parse(name, query, parameter_object_ids) {
  let oids =
    list.fold(parameter_object_ids, <<>>, fn(sum, oid) { <<sum:bits, oid:32>> })
  let len = list.length(parameter_object_ids)
  encode("P", <<
    encode_string(name):bits,
    encode_string(query):bits,
    len:16,
    oids:bits,
  >>)
}

fn encode_bind(
  portal,
  statement_name,
  parameter_format,
  parameters,
  result_format,
) {
  encode("B", <<
    portal:utf8,
    0,
    statement_name:utf8,
    0,
    encode_format_value(parameter_format):bits,
    parameters_to_bytes(parameters):bits,
    encode_format_value(result_format):bits,
  >>)
}

fn encode_function_call(
  object_id: Int,
  argument_format: FormatValue,
  arguments: ParameterValues,
  result_format: Format,
) {
  encode("F", <<
    object_id:32,
    encode_format_value(argument_format):bits,
    parameters_to_bytes(arguments):bits,
    encode_format(result_format):16,
  >>)
}

/// Send a message to the database
pub fn send_builder(conn: Connection, message) {
  case mug.send_builder(conn.socket, message) {
    Ok(Nil) -> Ok(conn)
    Error(err) -> Error(err)
  }
}

/// Send a message to the database
pub fn send(conn: Connection, message) {
  case mug.send(conn.socket, message) {
    Ok(Nil) -> Ok(conn)
    Error(err) -> Error(err)
  }
}

/// Receive a single message from the backend
pub fn receive(
  conn: Connection,
) -> Result(#(Connection, BackendMessage), ReadError) {
  case decode_backend_packet(conn.buffer) {
    Ok(#(message, rest)) -> Ok(#(with_buffer(conn, rest), message))
    Error(MessageIncomplete(_)) -> {
      case mug.receive(conn.socket, conn.timeout) {
        Ok(packet) ->
          receive(with_buffer(conn, <<conn.buffer:bits, packet:bits>>))
        Error(err) -> Error(SocketError(err))
      }
    }
    Error(err) -> Error(ReadDecodeError(err))
  }
}

fn with_buffer(conn: Connection, buffer: BitArray) {
  Connection(..conn, buffer:)
}

// decode a single message from the packet
pub fn decode_backend_packet(
  packet: BitArray,
) -> Result(#(BackendMessage, BitArray), MessageDecodingError) {
  case packet {
    <<message_type:bytes-size(1), length:32, tail:bytes>> -> {
      let len = length - 4
      case tail {
        <<data:bytes-size(len), next:bytes>> ->
          decode_backend_message(<<message_type:bits, data:bits>>)
          |> result.map(fn(msg) { #(msg, next) })
        _ -> Error(MessageIncomplete(tail))
      }
    }
    _ ->
      case bit_array.byte_size(packet) < 5 {
        True -> Error(MessageIncomplete(packet))
        False -> dec_err("packet size too small", packet)
      }
  }
}

// decode a backend message
pub fn decode_backend_message(binary) {
  case binary {
    <<"D":utf8, count:16, data:bytes>> -> decode_message_data_row(count, data)
    <<"E":utf8, data:bytes>> -> decode_error_response(data)
    <<"R":utf8, 0:32>> -> Ok(BeAuthenticationOk)
    <<"R":utf8, 2:32>> -> Ok(BeAuthenticationKerberosV5)
    <<"R":utf8, 3:32>> -> Ok(BeAuthenticationCleartextPassword)
    <<"R":utf8, 5:32, salt:bytes>> -> Ok(BeAuthenticationMD5Password(salt:))
    <<"R":utf8, 7:32>> -> Ok(BeAuthenticationGSS)
    <<"R":utf8, 8:32, auth_data:bytes>> ->
      Ok(BeAuthenticationGSSContinue(auth_data:))
    <<"R":utf8, 9:32>> -> Ok(BeAuthenticationSSPI)
    <<"R":utf8, 10:32, data:bytes>> -> decode_authentication_sasl(data)
    <<"R":utf8, 11:32, data:bytes>> -> Ok(BeAuthenticationSASLContinue(data:))
    <<"R":utf8, 12:32, data:bytes>> -> Ok(BeAuthenticationSASLFinal(data:))
    <<"K":utf8, pid:32, sk:32>> ->
      Ok(BeBackendKeyData(process_id: pid, secret_key: sk))
    <<"2":utf8>> -> Ok(BeBindComplete)
    <<"3":utf8>> -> Ok(BeCloseComplete)
    <<"C":utf8, data:bytes>> -> decode_command_complete(data)
    <<"d":utf8, data:bytes>> -> Ok(BeCopyData(data))
    <<"c":utf8>> -> Ok(BeCopyDone)
    <<"G":utf8, format:8, count:16, data:bytes>> ->
      decode_copy_response(In, format, count, data)
    <<"H":utf8, format:8, count:16, data:bytes>> ->
      decode_copy_response(Out, format, count, data)
    <<"W":utf8, format:8, count:16, data:bytes>> ->
      decode_copy_response(Both, format, count, data)
    <<"S":utf8, data:bytes>> -> decode_parameter_status(data)
    <<"v":utf8, version:32, count:32, data:bytes>> ->
      decode_negotiate_protocol_version(version, count, data)
    <<"n":utf8>> -> Ok(BeNoData)
    <<"N":utf8, data:bytes>> -> decode_notice_response(data)
    <<"A":utf8, process_id:32, data:bytes>> ->
      decode_notification_response(process_id, data)
    <<"t":utf8, count:16, data:bytes>> ->
      decode_parameter_description(count, data, [])
    <<"1":utf8>> -> Ok(BeParseComplete)
    <<"s":utf8>> -> Ok(BePortalSuspended)
    <<"T":utf8, count:16, data:bytes>> -> decode_row_descriptions(count, data)
    <<"Z":utf8, "I":utf8>> -> Ok(BeReadyForQuery(TransactionStatusIdle))
    <<"Z":utf8, "T":utf8>> ->
      Ok(BeReadyForQuery(TransactionStatusInTransaction))
    <<"Z":utf8, "E":utf8>> -> Ok(BeReadyForQuery(TransactionStatusFailed))
    _ -> Error(UnknownMessage(binary))
  }
}

/// decode a single message from the packet buffer.
/// note that FeCancelRequest, FeSslRequest, FeGssEncRequest, and
/// FeStartupMessage messages can only be decoded here because they don't follow
/// the standard message format.
pub fn decode_frontend_packet(
  packet: BitArray,
) -> Result(#(FrontendMessage, BitArray), MessageDecodingError) {
  case packet {
    <<16:32, 1234:16, 5678:16, process_id:32, secret_key:32, next:bytes>> ->
      Ok(#(FeCancelRequest(process_id:, secret_key:), next))
    <<8:32, 1234:16, 5679:16, next:bytes>> -> Ok(#(FeSslRequest, next))
    <<8:32, 1234:16, 5680:16, next:bytes>> -> Ok(#(FeGssEncRequest, next))
    // not sure if there's a way to use the `protocol_version` constant here
    <<length:32, 3:16, 0:16, next:bytes>> ->
      decode_startup_message(next, length - 8)
    <<message_type:bytes-size(1), length:32, tail:bytes>> -> {
      let len = length - 4
      case tail {
        <<data:bytes-size(len), next:bytes>> ->
          decode_frontend_message(<<message_type:bits, data:bits>>)
          |> result.map(fn(msg) { #(msg, next) })
        _ -> Error(MessageIncomplete(tail))
      }
    }
    <<_:48>> -> Error(MessageIncomplete(packet))
    _ -> dec_err("invalid message", packet)
  }
}

/// decode a frontend message (also see decode_frontend_packet for messages that
/// can't be decoded here)
pub fn decode_frontend_message(
  binary: BitArray,
) -> Result(FrontendMessage, MessageDecodingError) {
  case binary {
    <<"B":utf8, data:bytes>> -> decode_bind(data)
    <<"C":utf8, data:bytes>> -> decode_close(data)
    <<"d":utf8, data:bytes>> -> Ok(FeCopyData(data))
    <<"c":utf8>> -> Ok(FeCopyDone)
    <<"f":utf8, data:bytes>> -> decode_copy_fail(data)
    <<"D":utf8, data:bytes>> -> decode_describe(data)
    <<"E":utf8, data:bytes>> -> decode_execute(data)
    <<"H":utf8>> -> Ok(FeFlush)
    <<"F":utf8, data:bytes>> -> decode_function_call(data)
    <<"p":utf8, data:bytes>> -> Ok(FeAmbigous(FeGssResponse(data)))
    <<"P":utf8, data:bytes>> -> decode_parse(data)
    <<"Q":utf8, data:bytes>> -> decode_query(data)
    <<"S":utf8>> -> Ok(FeSync)
    <<"X":utf8>> -> Ok(FeTerminate)
    _ -> Error(UnknownMessage(data: binary))
  }
}

fn decode_startup_message(binary, size) {
  case binary {
    <<data:bytes-size(size), next:bytes>> ->
      decode_startup_message_pairs(data, [])
      |> result.map(fn(r) { #(r, next) })
    _ -> dec_err("invalid startup message", binary)
  }
}

fn decode_startup_message_pairs(binary, result) {
  case binary {
    <<0>> -> Ok(FeStartupMessage(params: list.reverse(result)))
    _ -> {
      use #(key, binary) <- try(decode_string(binary))
      use #(value, binary) <- try(decode_string(binary))
      decode_startup_message_pairs(binary, [#(key, value), ..result])
    }
  }
}

fn decode_query(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(query, rest) <- try(decode_string(binary))
  case rest {
    <<>> -> Ok(FeQuery(query))
    _ -> dec_err("Query message too long", binary)
  }
}

// FeQuery(query) -> encode("Q", encode_string(query))

fn decode_parse(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(name, binary) <- try(decode_string(binary))
  use #(query, binary) <- try(decode_string(binary))
  use parameter_object_ids <- try(decode_parameter_object_ids(binary))
  Ok(FeParse(name:, query:, parameter_object_ids:))
}

fn decode_parameter_object_ids(binary) {
  case binary {
    <<count:16, rest:bytes>> -> decode_parameter_object_ids_rec(rest, count, [])
    _ -> dec_err("expected object id count", binary)
  }
}

fn decode_parameter_object_ids_rec(binary, count, result) {
  case count, binary {
    0, <<>> -> Ok(list.reverse(result))
    _, <<id:32, rest:bytes>> ->
      decode_parameter_object_ids_rec(rest, count - 1, [id, ..result])
    _, _ -> dec_err("expected parameter object id", binary)
  }
}

fn decode_function_call(binary) -> Result(FrontendMessage, MessageDecodingError) {
  case binary {
    <<object_id:32, rest:bytes>> -> {
      use #(argument_format, rest) <- try(read_parameter_format(rest))
      use #(arguments, rest) <- try(read_parameters(rest, argument_format))
      use #(result_format, rest) <- try(read_format(rest))
      case rest {
        <<>> ->
          Ok(FeFunctionCall(
            argument_format:,
            arguments:,
            object_id:,
            result_format:,
          ))
        _ -> dec_err("invalid function call, data remains", rest)
      }
    }
    _ -> dec_err("invalid function call, no object id found", binary)
  }
}

fn decode_execute(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(portal, binary) <- try(decode_string(binary))
  case binary {
    <<count:32>> -> Ok(FeExecute(portal, count))
    _ -> dec_err("no execute return_row_count found", binary)
  }
}

fn decode_describe(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(what, binary) <- try(decode_what(binary))
  use #(name, binary) <- try(decode_string(binary))
  case binary {
    <<>> -> Ok(FeDescribe(what, name))
    _ -> dec_err("Describe message too long", binary)
  }
}

fn decode_copy_fail(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(error, binary) <- try(decode_string(binary))
  case binary {
    <<>> -> Ok(FeCopyFail(error))
    _ -> dec_err("CopyFail message too long", binary)
  }
}

fn decode_close(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(what, binary) <- try(decode_what(binary))
  use #(name, binary) <- try(decode_string(binary))
  case binary {
    <<>> -> Ok(FeClose(what, name))
    _ -> dec_err("Close message too long", binary)
  }
}

// FeClose(what, name) ->
//   encode("C", <<wire_what(what):bits, encode_string(name):bits>>)

fn decode_bind(binary) -> Result(FrontendMessage, MessageDecodingError) {
  use #(portal, binary) <- try(decode_string(binary))
  use #(statement_name, binary) <- try(decode_string(binary))
  use #(parameter_format, binary) <- try(read_parameter_format(binary))
  use #(parameters, binary) <- try(read_parameters(binary, parameter_format))
  use #(result_format, binary) <- try(read_parameter_format(binary))
  case binary {
    <<>> ->
      Ok(FeBind(
        portal:,
        statement_name:,
        parameter_format:,
        parameters:,
        result_format:,
      ))
    _ -> dec_err("Bind message too long", binary)
  }
}

fn decode_string(
  binary: BitArray,
) -> Result(#(String, BitArray), MessageDecodingError) {
  case binary_split(binary, <<0>>, []) {
    [head, tail] ->
      case bit_array.to_string(head) {
        Ok(str) -> Ok(#(str, tail))
        Error(Nil) -> dec_err("invalid string encoding", head)
      }
    _ -> dec_err("invalid string", binary)
  }
}

fn read_parameter_format(
  binary: BitArray,
) -> Result(#(FormatValue, BitArray), MessageDecodingError) {
  case binary {
    <<0:16, rest:bytes>> -> Ok(#(FormatAllText, rest))
    <<1:16, 0:16, rest:bytes>> -> Ok(#(FormatAll(Text), rest))
    <<1:16, 1:16, rest:bytes>> -> Ok(#(FormatAll(Binary), rest))
    <<n:16, rest:bytes>> -> read_wire_formats(n, rest, [])
    _ -> dec_err("invalid parameter format", binary)
  }
}

fn read_parameters(binary: BitArray, parameter_format: FormatValue) {
  case binary {
    <<count:16, rest:bytes>> -> {
      case parameter_format {
        FormatAllText -> list.repeat(Text, count)
        FormatAll(format) -> list.repeat(format, count)
        Formats(formats) -> formats
      }
      |> read_parameters_rec(count, rest, [])
    }
    _ -> dec_err("parameters without count", binary)
  }
}

fn read_parameters_rec(
  formats: List(Format),
  count: Int,
  binary: BitArray,
  result: List(ParameterValue),
) -> Result(#(List(ParameterValue), BitArray), MessageDecodingError) {
  let actual = list.length(formats)
  use <- bool.guard(
    actual != count,
    dec_err(
      "expected "
        <> int.to_string(count)
        <> " parameters, but got "
        <> int.to_string(actual),
      binary,
    ),
  )

  case count, formats, binary {
    0, _, rest -> Ok(#(list.reverse(result), rest))
    _, [_, ..rest_formats], <<-1:32-signed, rest:bytes>> -> {
      read_parameters_rec(rest_formats, count - 1, rest, [Null, ..result])
    }
    _, [format, ..rest_formats], <<len:32, value:bytes-size(len), rest:bytes>> ->
      read_parameters_rec(rest_formats, count - 1, rest, [
        read_parameter(format, value),
        ..result
      ])
    _, _, rest -> dec_err("invalid parameter value", rest)
  }
}

fn read_parameter(format: Format, value: BitArray) {
  case format {
    Text -> Parameter(value)
    Binary -> Parameter(value)
  }
}

fn read_wire_formats(
  count: Int,
  binary: BitArray,
  result: List(Format),
) -> Result(#(FormatValue, BitArray), MessageDecodingError) {
  case count, binary {
    0, _ -> Ok(#(Formats(list.reverse(result)), binary))
    _, <<0:16, rest:bytes>> ->
      read_wire_formats(count - 1, rest, [Text, ..result])
    _, <<1:16, rest:bytes>> ->
      read_wire_formats(count - 1, rest, [Binary, ..result])
    _, _ -> dec_err("unknown format", binary)
  }
}

fn decode_command_complete(
  binary: BitArray,
) -> Result(BackendMessage, MessageDecodingError) {
  {
    use fine <- try(case binary {
      <<"INSERT 0 ":utf8, rows:bytes>> -> Ok(#(Insert, rows))
      <<"DELETE ":utf8, rows:bytes>> -> Ok(#(Delete, rows))
      <<"UPDATE ":utf8, rows:bytes>> -> Ok(#(Update, rows))
      <<"MERGE ":utf8, rows:bytes>> -> Ok(#(Merge, rows))
      <<"SELECT ":utf8, rows:bytes>> -> Ok(#(Select, rows))
      <<"MOVE ":utf8, rows:bytes>> -> Ok(#(Move, rows))
      <<"FETCH ":utf8, rows:bytes>> -> Ok(#(Fetch, rows))
      <<"COPY ":utf8, rows:bytes>> -> Ok(#(Copy, rows))
      _ -> dec_err("invalid command", binary)
    })

    let #(command, rows_raw) = fine
    let len = bit_array.byte_size(rows_raw) - 1

    use rows_bits <- try(case rows_raw {
      <<rows_bits:bytes-size(len), 0>> -> Ok(rows_bits)
      _ -> dec_err("invalid command row count", binary)
    })

    use rows_string <- try(
      bit_array.to_string(rows_bits)
      |> result.replace_error(msg_dec_err(
        "failed to convert row count to string",
        rows_bits,
      )),
    )

    use rows <- try(
      int.parse(rows_string)
      |> result.replace_error(msg_dec_err(
        "failed to convert row count to int",
        rows_bits,
      )),
    )

    Ok(BeCommandComplete(command, rows))
  }
  |> result.or(Ok(BeCommandComplete(Insert, -1)))
}

pub type TransactionStatus {
  TransactionStatusIdle
  TransactionStatusInTransaction
  TransactionStatusFailed
}

fn decode_parameter_description(count, binary, results) {
  case count, binary {
    0, <<>> -> Ok(BeParameterDescription(list.reverse(results)))
    _, <<value:32, tail:bytes>> ->
      decode_parameter_description(count - 1, tail, [value, ..results])
    _, _ -> dec_err("invalid parameter description", binary)
  }
}

fn decode_notification_response(
  process_id,
  binary,
) -> Result(BackendMessage, MessageDecodingError) {
  use strings <- try(read_strings(binary, 2, []))
  case strings {
    [channel, payload] ->
      Ok(BeNotificationResponse(process_id:, channel:, payload:))
    _ -> dec_err("invalid notification response encoding", binary)
  }
}

fn decode_negotiate_protocol_version(version, count, binary) {
  use options <- try(read_strings(binary, count, []))
  Ok(BeNegotiateProtocolVersion(version, options))
}

fn decode_row_descriptions(count, binary) {
  use fields <- try(read_row_descriptions(count, binary, []))
  Ok(BeRowDescriptions(fields))
}

fn decode_parameter_status(binary) {
  use strings <- try(decode_strings(binary))
  case strings {
    [name, value] -> Ok(BeParameterStatus(name:, value:))
    _ -> dec_err("invalid parameter status", binary)
  }
}

fn decode_authentication_sasl(binary) {
  use strings <- try(decode_strings(binary))
  Ok(BeAuthenticationSASL(strings))
}

fn decode_copy_response(direction, format_raw, count, rest) {
  use overall_format <- try(decode_format(format_raw))
  use <- bool.guard(
    bit_array.byte_size(rest) != count * 2,
    dec_err("size must be count * 2", rest),
  )

  use codes <- try(decode_format_codes(rest, []))

  case overall_format == Text {
    False -> Ok(BeCopyResponse(direction, overall_format, codes))
    True ->
      case list.all(codes, fn(code) { code == Text }) {
        True -> Ok(BeCopyResponse(direction, overall_format, codes))
        False -> dec_err("invalid copy response format", rest)
      }
  }
}

fn decode_format_codes(
  binary: BitArray,
  result: List(Format),
) -> Result(List(Format), MessageDecodingError) {
  case binary {
    <<code:16, tail:bytes>> ->
      case decode_format(code) {
        Ok(format) -> decode_format_codes(tail, [format, ..result])
        Error(err) -> Error(err)
      }
    <<>> -> Ok(list.reverse(result))
    _ -> dec_err("invalid format codes", binary)
  }
}

fn decode_format(num: Int) {
  case num {
    0 -> Ok(Text)
    1 -> Ok(Binary)
    _ -> dec_err("invalid format code: " <> int.to_string(num), <<>>)
  }
}

fn read_format(binary) {
  case binary {
    <<0:16, rest:bytes>> -> Ok(#(Text, rest))
    <<1:16, rest:bytes>> -> Ok(#(Binary, rest))
    _ -> dec_err("invalid format code", binary)
  }
}

fn encode_format(format_raw) -> Int {
  case format_raw {
    Text -> 0
    Binary -> 1
  }
}

pub type DataRow {
  DataRow(List(BitArray))
}

fn decode_message_data_row(
  count,
  rest,
) -> Result(BackendMessage, MessageDecodingError) {
  case decode_message_data_row_rec(rest, count, []) {
    Ok(cols) ->
      case list.length(cols) == count {
        True -> Ok(BeMessageDataRow(cols))
        False -> dec_err("column count doesn't match", rest)
      }
    Error(err) -> Error(err)
  }
}

fn decode_message_data_row_rec(
  binary,
  count,
  result,
) -> Result(List(BitArray), MessageDecodingError) {
  case count, binary {
    0, _ -> Ok(list.reverse(result))
    _, <<length:32, value:bytes-size(length), rest:bytes>> ->
      decode_message_data_row_rec(rest, count - 1, [value, ..result])
    _, _ ->
      dec_err(
        "failed to parse data row at count " <> int.to_string(count),
        binary,
      )
  }
}

pub type RowDescriptionField {
  RowDescriptionField(
    // The field name.
    name: String,
    // If the field can be identified as a column of a specific table, the
    // object ID of the table; otherwise zero.
    table_oid: Int,
    // If the field can be identified as a column of a specific table, the
    // attribute number of the column; otherwise zero.
    attr_number: Int,
    // The object ID of the field's data type.
    data_type_oid: Int,
    // The data type size (see pg_type.typlen). Note that negative values denote
    // variable-width types.
    data_type_size: Int,
    // The type modifier (see pg_attribute.atttypmod). The meaning of the
    // modifier is type-specific.
    type_modifier: Int,
    // The format code being used for the field. Currently will be zero (text)
    // or one (binary). In a RowDescription returned from the statement variant
    // of Describe, the format code is not yet known and will always be zero.
    format_code: Int,
  )
}

fn read_row_descriptions(count, binary, result) {
  case count, binary {
    0, <<>> -> Ok(list.reverse(result))
    _, <<>> -> dec_err("row description count mismatch", binary)
    _, _ ->
      case read_row_description_field(binary) {
        Ok(#(field, tail)) ->
          read_row_descriptions(count - 1, tail, [field, ..result])
        Error(err) -> Error(err)
      }
  }
}

fn read_row_description_field(binary) {
  case read_string(binary) {
    Ok(#(
      name,
      <<
        table_oid:32,
        attr_number:16,
        data_type_oid:32,
        data_type_size:16,
        type_modifier:32,
        format_code:16,
        tail:bytes,
      >>,
    )) ->
      Ok(#(
        RowDescriptionField(
          name:,
          table_oid:,
          attr_number:,
          data_type_oid:,
          data_type_size:,
          type_modifier:,
          format_code:,
        ),
        tail,
      ))
    Ok(#(_, tail)) -> dec_err("failed to parse row description field", tail)
    Error(_) -> dec_err("failed to decode row description field name", binary)
  }
}

fn decode_strings(binary) {
  let length = bit_array.byte_size(binary) - 1
  case binary {
    <<>> -> Ok([])
    <<head:bytes-size(length), 0>> -> {
      binary_split(head, <<0>>, [Global])
      |> list.map(bit_array.to_string)
      |> result.all()
      |> result.replace_error(msg_dec_err("invalid strings encoding", binary))
    }
    _ -> dec_err("string size didn't match", binary)
  }
}

fn read_strings(binary, count, result) {
  case count {
    0 -> Ok(list.reverse(result))
    _ -> {
      case read_string(binary) {
        Ok(#(value, rest)) -> read_strings(rest, count - 1, [value, ..result])
        Error(err) -> Error(err)
      }
    }
  }
}

fn read_string(binary) {
  case binary_split(binary, <<0>>, []) {
    [<<>>, <<>>] -> Ok(#("", <<>>))
    [head, tail] -> {
      bit_array.to_string(head)
      |> result.replace_error(msg_dec_err("invalid string encoding", head))
      |> result.map(fn(s) { #(s, tail) })
    }
    _ -> dec_err("invalid string", binary)
  }
}

fn decode_notice_response(binary) {
  use fields <- try(decode_fields(binary))
  Ok(BeNoticeResponse(fields))
}

fn decode_error_response(binary) {
  use fields <- try(decode_fields(binary))
  Ok(BeErrorResponse(fields))
}

pub type ErrorOrNoticeField {
  Code(String)
  Detail(String)
  File(String)
  Hint(String)
  Line(String)
  Message(String)
  Position(String)
  Routine(String)
  SeverityLocalized(String)
  Severity(String)
  Where(String)
  Column(String)
  DataType(String)
  Constraint(String)
  InternalPosition(String)
  InternalQuery(String)
  Schema(String)
  Table(String)
  Unknown(key: BitArray, value: String)
}

fn decode_fields(binary) {
  case decode_fields_rec(binary, []) {
    Ok(fields) ->
      fields
      |> list.map(fn(key_value_raw) {
        let #(key, value) = key_value_raw
        case key {
          <<"S":utf8>> -> Severity(value)
          <<"V":utf8>> -> SeverityLocalized(value)
          <<"C":utf8>> -> Code(value)
          <<"M":utf8>> -> Message(value)
          <<"D":utf8>> -> Detail(value)
          <<"H":utf8>> -> Hint(value)
          <<"P":utf8>> -> Position(value)
          <<"p":utf8>> -> InternalPosition(value)
          <<"q":utf8>> -> InternalQuery(value)
          <<"W":utf8>> -> Where(value)
          <<"s":utf8>> -> Schema(value)
          <<"t":utf8>> -> Table(value)
          <<"c":utf8>> -> Column(value)
          <<"d":utf8>> -> DataType(value)
          <<"n":utf8>> -> Constraint(value)
          <<"F":utf8>> -> File(value)
          <<"L":utf8>> -> Line(value)
          <<"R":utf8>> -> Routine(value)
          _ -> Unknown(key, value)
        }
      })
      |> set.from_list()
      |> Ok
    Error(err) -> Error(err)
  }
}

fn decode_fields_rec(binary, result) {
  case binary {
    <<0>> | <<>> -> Ok(result)
    <<field_type:bytes-size(1), rest:bytes>> -> {
      case binary_split(rest, <<0>>, []) {
        [head, tail] -> {
          case bit_array.to_string(head) {
            Ok(value) ->
              decode_fields_rec(tail, [#(field_type, value), ..result])
            Error(Nil) ->
              // Sometimes Postgres can reply with a string that is not utf8
              // encoded, in that case we try our best to still get something
              // out of it.
              // If all fails, we still return an error.
              case recover_string(head) {
                Ok(value) ->
                  decode_fields_rec(tail, [#(field_type, value), ..result])
                Error(Nil) -> dec_err("invalid field encoding", binary)
              }
          }
        }
        _ -> dec_err("invalid field separator", binary)
      }
    }
    _ -> dec_err("invalid field", binary)
  }
}

@external(erlang, "squirrel_ffi", "recover_string")
fn recover_string(value: a) -> Result(String, Nil)

type BinarySplitOption {
  Global
}

@external(erlang, "binary", "split")
fn binary_split(
  subject: BitArray,
  pattern: BitArray,
  options: List(BinarySplitOption),
) -> List(BitArray)
