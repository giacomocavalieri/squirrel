//// In this module lies the core of `squirrel`.
//// It exposes a single public function called `main` that is used to turn a
//// list of untyped queries into typed ones.
////
//// To do so, `squirrel` will try to connect to a database, have it parse the
//// queries and reply with the types it could infer.
//// Then it's as simple as (not that simple in practice 😆) converting the
//// Postgres types into Gleam types.
////
//// > 💡 I tried to do my best to comment everything as much as possible and
//// > make things easy to read.
//// > If you feel something is poorly commented or hard to understand, then
//// > that is a bug! Please do reach out, I'd love to hear your feedback.
////

import eval
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeErrors, type Dynamic} as d
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import squirrel/internal/database/postgres_protocol as pg
import squirrel/internal/error.{
  type Error, type Pointer, type ValueIdentifierError, ByteIndex,
  CannotParseQuery, PgCannotDecodeReceivedMessage, PgCannotDescribeQuery,
  PgCannotEstablishTcpConnection, PgCannotReceiveMessage, PgCannotSendMessage,
  PgInvalidPassword, PgInvalidSha256ServerProof, PgInvalidUserDatabase,
  PgPermissionDenied, PgUnexpectedAuthMethodMessage,
  PgUnexpectedCleartextAuthMessage, PgUnexpectedSha256AuthMessage,
  PgUnsupportedAuthentication, Pointer, QueryHasInvalidColumn,
  QueryHasUnsupportedType,
}
import squirrel/internal/eval_extra
import squirrel/internal/gleam
import squirrel/internal/query.{
  type TypedQuery, type UntypedQuery, TypedQuery, UntypedQuery,
}
import squirrel/internal/scram

const find_postgres_type_query = "
select
  -- The name of the type or, if the type is an array, the name of its
  -- elements' type.
  case
    when elem.typname is null then type.typname
    else elem.typname
 	end as type,
  -- Tells us how to interpret the firs column: if this is true then the first
  -- column is the type of the elements of the array type.
  -- Otherwise it means we've found a base type.
  case
    when elem.typname is null then false
    else true
 	end as is_array
from
  pg_type as type
  left join pg_type as elem on type.typelem = elem.oid
where
  type.oid = $1
"

const find_column_nullability_query = "
select
  -- Whether the column has a not-null constraint.
  attnotnull
from
  pg_attribute
where
  -- The oid of the table the column comes from.
  attrelid = $1
  -- The index of the column we're looking for.
  and attnum = $2
"

// --- TYPES -------------------------------------------------------------------

/// A Postgres type.
///
/// > ⚠️Postgres has loads of types and this might not cover the more exotic
/// > ones but for now it feels more than enough.
///
type PgType {
  /// A base type, like `integer`, `text`, `char`, ...
  ///
  PBase(name: String)

  /// An array type like `int[]`, `text[]`, ...
  ///
  PArray(inner: PgType)

  /// A type that could also be `NULL`, this is particularly common for columns
  /// that do not have a `not null` constraint; or for those coming from partial
  /// joins.
  ///
  POption(inner: PgType)
}

/// The context in which all database-related actions will take place.
///
type Context {
  Context(
    /// A connection to the database. Squirrel does nothing fancy and just uses
    /// a single connection to run all the queries.
    ///
    db: pg.Connection,
    /// A cache from `oid` to corresponding Gleam type.
    /// We use this to avoid having to reach to the database every time we need
    /// to infer a type.
    ///
    /// > 💡 An oid is an integer identifier that is used by Postgres to
    /// > uniquely identify types (and a lot of other various objects, see
    /// > [this documentation page](https://www.postgresql.org/docs/current/datatype-oid.html)).
    ///
    gleam_types: Dict(Int, gleam.Type),
    /// A cache from table `oid` and column index to its nullability.
    /// We use this to avoid having to reach to the database every time we need
    /// to type a column.
    ///
    column_nullability: Dict(#(Int, Int), Nullability),
  )
}

/// Information about a column's nullability.
/// If a column has a `not null` constraint then it will be `NotNullable`, in
/// all other cases it will be `Nullable`.
///
/// > ⚠️ A column with a `not null` constraint might still be considered
/// > nullable if it comes from a left/right join!
///
type Nullability {
  Nullable
  NotNullable
}

/// A query plan produced by Postgres when we ask it to `explain` a query.
///
type Plan {
  Plan(
    join_type: Option(JoinType),
    parent_relation: Option(ParentRelation),
    output: Option(List(String)),
    plans: Option(List(Plan)),
  )
}

type JoinType {
  Full
  Left
  Right
  Other
}

type ParentRelation {
  Inner
  NotInner
}

/// This is the type of a database-related action.
/// In order to be carried out it needs to have access to the database `Context`
/// and could fail with an `Error`.
///
type Db(a) =
  eval.Eval(a, Error, Context)

/// The options used to establish a connection to the Postgres database.
///
pub type ConnectionOptions {
  ConnectionOptions(
    host: String,
    port: Int,
    user: String,
    password: String,
    database: String,
    timeout: Int,
  )
}

// --- POSTGRES TO GLEAM TYPES CONVERSIONS -------------------------------------

/// This function turns a Postgres type into a Gleam one, returning an error
/// with the type name if it is not currently supported.
///
fn pg_to_gleam_type(type_: PgType) -> Result(gleam.Type, String) {
  case type_ {
    PArray(inner:) ->
      pg_to_gleam_type(inner)
      |> result.map(gleam.List)
      |> result.map_error(fn(inner) { inner <> "[]" })

    POption(inner:) ->
      pg_to_gleam_type(inner)
      |> result.map(gleam.Option)
      |> result.map_error(fn(inner) { inner <> "?" })

    PBase(name:) ->
      case name {
        "bool" -> Ok(gleam.Bool)
        "text" | "char" | "bpchar" | "varchar" -> Ok(gleam.String)
        "float4" | "float8" | "numeric" -> Ok(gleam.Float)
        "int2" | "int4" | "int8" -> Ok(gleam.Int)
        "json" | "jsonb" -> Ok(gleam.Json)
        "uuid" -> Ok(gleam.Uuid)
        "bytea" -> Ok(gleam.BitArray)
        "date" -> Ok(gleam.Date)
        "timestamp" -> Ok(gleam.Timestamp)
        _ -> Error(name)
      }
  }
}

// --- CLI ENTRY POINT ---------------------------------------------------------

/// Connects to a Postgres database (using the given options) and types a list
/// of queries.
///
/// This might fail with an `Error` if a database connection cannot be
/// established, making it impossible to type any of the queries.
/// Otherwise, it will try typing all the queries, retuning a list of typed ones
/// and a list of possible errors for the ones it couldn't type.
///
pub fn main(
  queries: List(UntypedQuery),
  connection: ConnectionOptions,
) -> Result(#(List(TypedQuery), List(Error)), Error) {
  use db <- result.try(
    pg.connect(connection.host, connection.port, connection.timeout)
    |> result.map_error(PgCannotEstablishTcpConnection(
      host: connection.host,
      port: connection.port,
      reason: _,
    )),
  )

  let context =
    Context(db:, gleam_types: dict.new(), column_nullability: dict.new())

  // Once the server has confirmed that it is ready to accept query requests we
  // can start gathering information about all the different queries.
  // After each one we need to make sure the server is ready to go on with the
  // next one.
  //
  //     https://www.postgresql.org/docs/current/protocol-flow.html#PROTOCOL-FLOW-EXT-QUERY
  //
  let #(context, connection) = eval.step(authenticate(connection), context)
  case connection {
    Error(error) -> Error(error)
    // After successfully authenticating we can try and type all the queries.
    Ok(_) ->
      list.map(queries, infer_types)
      |> eval_extra.run_all(context)
      |> result.partition
      |> Ok
  }
}

fn authenticate(connection: ConnectionOptions) -> Db(Nil) {
  let ConnectionOptions(user:, database:, password:, ..) = connection

  let params = [#("user", user), #("database", database)]
  use _ <- eval.try(send(pg.FeStartupMessage(params)))

  use msg <- eval.try(receive())
  use _ <- eval.try(case msg {
    pg.BeAuthenticationOk -> eval.return(Nil)
    pg.BeAuthenticationCleartextPassword ->
      cleartext_authenticate(user, password)
    pg.BeAuthenticationMD5Password(_) -> unsupported_authentication("md5")
    pg.BeAuthenticationGSS -> unsupported_authentication("GSS")
    pg.BeAuthenticationSASL(methods) ->
      // Methods is a list of the possible authentication methods, for now we
      // only support SCRAM-SHA-256.
      case list.contains(methods, "SCRAM-SHA-256") {
        True -> sha_256_authenticate(user, password)
        // Any other method is not supported and we will report it as such
        // (we need to filter away the empty strings from the `method` list
        // since sometimes the server sends an unexpected empty string we don't
        // want to appear in the error).
        _ ->
          case list.filter(methods, keeping: fn(method) { method != "" }) {
            [_, ..] -> "SASL(" <> string.join(methods, ",") <> ")"
            [] -> "SASL"
          }
          |> unsupported_authentication
      }
    pg.BeAuthenticationSSPI -> unsupported_authentication("SSPI")
    pg.BeAuthenticationKerberosV5 -> unsupported_authentication("KerberosV5")
    _ -> unexpected_message(PgUnexpectedAuthMethodMessage, "AuthMethod", msg)
  })

  use _ <- eval.try(
    wait_until_ready()
    // In case there's a receive error while waiting for the server to be ready
    // we want to display a more helpful error message because the problem here
    // must be with an invalid username/database combination.
    |> eval.replace_error(PgInvalidUserDatabase(
      user: connection.user,
      database: connection.database,
    )),
  )

  eval.return(Nil)
}

fn cleartext_authenticate(user: String, password: String) -> Db(Nil) {
  use _ <- eval.try(send(pg.FeAmbigous(pg.FePasswordMessage(password))))
  use msg <- eval.try(receive())
  case msg {
    pg.BeAuthenticationOk -> eval.return(Nil)
    pg.BeErrorResponse(_) -> eval.throw(PgInvalidPassword(user:))

    // The response should only ever be Ok or Error (in case the password is
    // not correct).
    _ ->
      unexpected_message(
        PgUnexpectedCleartextAuthMessage,
        "AuthenticationOk ok ErrorRespose",
        msg,
      )
  }
}

fn sha_256_authenticate(user: String, password: String) -> Db(Nil) {
  // The authentication works as follow:
  // - We create a client `nonce` to include in the first message
  // - We send a `ClientFirst` message asking
  let nonce = scram.nonce()
  let client_first_msg = scram.ClientFirst(user:, nonce:)
  use _ <- eval.try(
    scram.encode_client_first(client_first_msg)
    |> pg.FeSaslInitialResponse("SCRAM-SHA-256", _)
    |> pg.FeAmbigous
    |> send,
  )
  // - If the server has no problems with the first message we will receive an
  //   ok from the server to go on with the authentication.
  // - We decode that message to get a hold of various server-sent parameters
  //   like its own nonce, the number of iterations to use to salt the password,
  //   and the salt to use
  use msg <- eval.try(receive())
  use raw_server_first_msg <- eval.try(expect_sasl_continue_message(msg))
  let assert Ok(server_first_msg) =
    scram.parse_server_first(raw_server_first_msg, nonce)
  // - We then send a last message including our password.
  // - When encoding the message to send we also get back the expected server
  //   proof we need to get back from the server to be sure authentication was
  //   ok.
  let client_last_msg =
    scram.ClientLast(client_first_msg, server_first_msg, password)
  let #(client_last_msg, expected_server_proof) =
    scram.encode_client_last(client_last_msg)
  use _ <- eval.try(
    client_last_msg
    |> pg.FeSaslResponse
    |> pg.FeAmbigous
    |> send,
  )
  // - At this point can either respond with an ok (signalling the
  //   authentication was ok), or with an error.
  //   A `28P01` error means the password is invalid, any other error signals
  //   something was wrong with our implementation of the protocol and is a bug.
  // - If the response is ok we need to check that the proof the server sent us
  //   is the one we were expecting to receive.
  use msg <- eval.try(receive())
  use msg <- eval.try(expect_sasl_final_message(msg, user))
  check_sasl_final_message(msg, expected_server_proof, user)
}

/// Receive a message and expect it to be a SASLContinue message, returning its
/// content.
///
fn expect_sasl_continue_message(msg: pg.BackendMessage) -> Db(BitArray) {
  case msg {
    pg.BeAuthenticationSASLContinue(server_first) -> eval.return(server_first)
    _ ->
      unexpected_message(
        PgUnexpectedSha256AuthMessage,
        "AuthenticationSASLContinue(server-first)",
        msg,
      )
  }
}

/// Received a message and expects it to be a SASLFinal message, returning its
/// content.
///
/// If the message is an `ErrorResponse` with the invalid password error code,
/// it throws an `InvalidPassword` error, failing the authentication.
///
fn expect_sasl_final_message(msg: pg.BackendMessage, user: String) {
  let unexpected_message =
    unexpected_message(
      PgUnexpectedSha256AuthMessage,
      "AuthenticationSASLFinal or BeErrorResponse",
      msg,
    )

  case msg {
    pg.BeAuthenticationSASLFinal(msg) -> eval.return(msg)
    pg.BeErrorResponse(fields) ->
      case set.contains(fields, pg.Code("28P01")) {
        True -> eval.throw(PgInvalidPassword(user))
        False -> unexpected_message
      }
    _ -> unexpected_message
  }
}

/// Checs that the SASLFinal message content is valid and has the expected
/// server proof.
///
fn check_sasl_final_message(
  msg: BitArray,
  expected_server_proof: BitArray,
  user: String,
) -> Db(Nil) {
  case scram.parse_server_final(msg) {
    Error(Nil) -> eval.throw(PgInvalidSha256ServerProof)
    Ok(scram.Failed(_)) -> eval.throw(PgInvalidPassword(user))
    Ok(scram.Successful(server_proof)) ->
      case server_proof == expected_server_proof {
        True -> eval.return(Nil)
        False -> eval.throw(PgInvalidSha256ServerProof)
      }
  }
}

/// Returns type information about a query.
///
fn infer_types(query: UntypedQuery) -> Db(TypedQuery) {
  // Postgres doesn't give us 100% accurate data regardin a query's type.
  // We'll need to perform a couple of database interrogations and do some
  // guessing.
  //
  // The big picture idea is the following:
  // - We ask the server to prepare the query.
  // - Postgres will reply with type information about the returned rows and the
  //   query's parameters.
  let action = parameters_and_returns(query)
  use #(parameters, returns) <- eval.try(action)
  // - The parameters' types are just OIDs so we need to interrogate the
  //   database to learn the actual corresponding Gleam type.
  use parameters <- eval.try(resolve_parameters(query, parameters))
  // - The returns' types are just OIDs so we have to do the same for those.
  // - Here comes the tricky part: we can't know if a column is nullable just
  //   from the server's previous answer.
  //   - For columns coming from a database table we'll look it up and see if
  //     the column is nullable or not
  //   - But this is not enough! If a returned column comes from a left/right
  //     join it will be nullable even if it is not in the original table.
  //     To work around this we'll have to inspect the query plan.
  use plan <- eval.try(query_plan(query, list.length(parameters)))
  let nullables = nullables_from_plan(plan)
  use returns <- eval.try(resolve_returns(query, returns, nullables))

  query
  |> query.add_types(parameters, returns)
  |> eval.return
}

fn parameters_and_returns(query: UntypedQuery) -> Db(_) {
  // We need to send three messages:
  // - `Parse` with the query to parse
  // - `Describe` to ask the server to reply with a description of the query's
  //   return types and parameter types. This is what we need to understand the
  //   SQL inferred types and generate the corresponding Gleam types.
  // - `Sync` to ask the server to immediately reply with the results of parsing
  //
  use _ <- eval.try(
    send_all([
      pg.FeParse("", query.content, []),
      pg.FeDescribe(pg.PreparedStatement, ""),
      pg.FeSync,
    ]),
  )

  // Error builder used in the following steps in case the message sequence
  // doesn't go as planned.
  let cannot_describe = fn(expected, got) {
    PgCannotDescribeQuery(
      file: query.file,
      query_name: gleam.identifier_to_string(query.name),
      expected:,
      got:,
    )
  }

  use msg <- eval.try(receive())
  case msg {
    pg.BeErrorResponse(errors) ->
      eval.throw(error_fields_to_parse_error(query, errors))
    pg.BeParseComplete -> {
      use msg <- eval.try(receive())
      use parameters <- eval.try(case msg {
        pg.BeParameterDescription(parameters) -> eval.return(parameters)
        pg.BeNoData -> eval.return([])
        _ -> unexpected_message(cannot_describe, "ParameterDescription", msg)
      })

      use msg <- eval.try(receive())
      use rows <- eval.try(case msg {
        pg.BeRowDescriptions(rows) -> eval.return(rows)
        pg.BeNoData -> eval.return([])
        _ -> unexpected_message(cannot_describe, "RowDescriptions", msg)
      })

      use msg <- eval.try(receive())
      use _ <- eval.try(case msg {
        pg.BeReadyForQuery(_) -> eval.return(Nil)
        _ -> unexpected_message(cannot_describe, "ReadyForQuery", msg)
      })

      eval.return(#(parameters, rows))
    }
    _ ->
      unexpected_message(cannot_describe, "ParseComplete or ErrorResponse", msg)
  }
}

/// Given an untyped query and the error fields we got back from the database in
/// case it couldn't be parsed, produces an appropriate `Error`.
///
fn error_fields_to_parse_error(
  query: UntypedQuery,
  errors: Set(pg.ErrorOrNoticeField),
) -> Error {
  // We first look for the relevant errors in the set of errors the database
  // returned. This way we can attach additional information explaining why the
  // query failed.
  let #(error_code, message, position, hint) = {
    use acc, error_field <- set.fold(errors, from: #(None, None, None, None))
    let #(code, message, position, hint) = acc
    case error_field {
      pg.Code(code) -> #(Some(code), message, position, hint)
      pg.Message(message) -> #(code, Some(message), position, hint)
      pg.Hint(hint) -> #(code, message, position, Some(hint))
      pg.Position(position) ->
        case int.parse(position) {
          Ok(position) -> #(code, message, Some(position), hint)
          Error(_) -> acc
        }
      _ -> acc
    }
  }

  // If we found both a `Message` and `Position` error messages then we can turn
  // those into a pointer that will be shown in the error message.
  let #(pointer, additional_error_message) = case message, position {
    // In case the message is tagged with a position we want it to be a pointer
    // and do not have any additional error message.
    Some(message), Some(position) -> #(
      Some(Pointer(point_to: ByteIndex(position), message:)),
      None,
    )

    // If it doesn't have any position then we do not have a pointer but still
    // report the error as an additional message.
    Some(message), None -> #(None, Some(message))

    _, _ -> #(None, None)
  }

  cannot_parse_error(
    query,
    error_code:,
    hint:,
    additional_error_message:,
    pointer:,
  )
}

fn resolve_parameters(
  query: UntypedQuery,
  parameters: List(Int),
) -> Db(List(gleam.Type)) {
  use oid <- eval_extra.try_map(parameters)
  find_gleam_type(query, oid)
}

/// Looks up a type with the given id in the Postgres registry.
///
/// > ⚠️ This function assumes that the oid is present in the database and
/// > will crash otherwise. This should only be called with oids coming from
/// > a database interrogation.
///
fn find_gleam_type(query: UntypedQuery, oid: Int) -> Db(gleam.Type) {
  // We first look for the Gleam type corresponding to this id in the cache to
  // avoid hammering the db with needless queries.
  use <- with_cached_gleam_type(oid)

  // The only parameter to this query is the oid of the type to lookup:
  // that's a 32bit integer (its oid needed to prepare the query is 23).
  let params = [pg.Parameter(<<oid:32>>)]
  let run_query =
    find_postgres_type_query
    |> run_query(query.file, params, [23])

  use res <- eval.try(run_query)

  // We know the output must only contain two values: the name and a boolean to
  // check wether it is an array or not.
  // It's safe to assert because this query is hard coded in our code and the
  // output shape cannot change without us changing that query.
  let assert [name, is_array] = res

  // We then decode the bitarrays we got as a result:
  // - `name` is just a string
  // - `is_array` is a pg boolean
  let assert Ok(name) = bit_array.to_string(name)
  let type_ = case bit_array_to_bool(is_array) {
    True -> PArray(PBase(name))
    False -> PBase(name)
  }

  pg_to_gleam_type(type_)
  |> result.map_error(unsupported_type_error(query, _))
  |> eval.from_result
}

/// Returns the query plan for a given query.
/// `parameters` is the number of parameter placeholders in the query.
///
fn query_plan(query: UntypedQuery, parameters: Int) -> Db(Plan) {
  // We ask postgres to give us the query plan. To do that we need to fill in
  // all the possible holes in the user supplied query with null values;
  // otherwise, the server would complain that it has arguments that are not
  // bound.
  let explain_query = "explain (format json, verbose) " <> query.content
  let params = list.repeat(pg.Null, parameters)
  let run_query = run_query(explain_query, query.file, params, [])
  use res <- eval.try(run_query)

  // We know the output will only contain a single row that is the json string
  // containing the query plan.
  let assert [plan] = res
  let assert Ok([plan, ..]) = json.decode_bits(plan, json_plans_decoder)
  eval.return(plan)
}

/// Given a query plan, returns a set with the indices of the output columns
/// that can contain null values.
///
fn nullables_from_plan(plan: Plan) -> Set(Int) {
  let outputs = case plan.output {
    Some(outputs) -> list.index_fold(outputs, dict.new(), dict.insert)
    None -> dict.new()
  }

  do_nullables_from_plan(plan, outputs, set.new())
}

fn do_nullables_from_plan(
  plan: Plan,
  // A dict from "column name" to its position in the query output.
  query_outputs: Dict(String, Int),
  nullables: Set(Int),
) -> Set(Int) {
  let nullables = case plan.output, plan.join_type, plan.parent_relation {
    // - All the outputs of a full join must be marked as nullable
    // - All the outputs of an inner half join must be marked as nullable
    Some(outputs), Some(Full), _ | Some(outputs), _, Some(Inner) -> {
      use nullables, output <- list.fold(outputs, from: nullables)
      case dict.get(query_outputs, output) {
        Ok(i) -> set.insert(nullables, i)
        Error(_) -> nullables
      }
    }
    _, _, _ -> nullables
  }

  case plan.plans, plan.join_type {
    // If this is an inner half join we keep inspecting the children to mark
    // their outputs as nullable.
    Some(plans), Some(Left) | Some(plans), Some(Right) -> {
      use nullables, plan <- list.fold(plans, from: nullables)
      do_nullables_from_plan(plan, query_outputs, nullables)
    }
    _, _ -> nullables
  }
}

/// Given a list of `RowDescriptionFields` it turns those into Gleam fields with
/// a name and a type.
///
/// This also uses nullability info coming from the query plan to figure out if
/// a column can be nullable or not:
/// - If the column name ends with `!` it will be forced to be not nullable
/// - If the column name ends with `?` it will be forced to be nullable
/// - If the column appears in the `nullables` set then it will be nullable
/// - Othwerwise we look for its metadata in the database and if it has a
///   not-null constraint it will be not nullable; otherwise it will be nullable
///
fn resolve_returns(
  query: UntypedQuery,
  returns: List(pg.RowDescriptionField),
  nullables: Set(Int),
) -> Db(List(gleam.Field)) {
  use column, i <- eval_extra.try_index_map(returns)
  let pg.RowDescriptionField(
    data_type_oid: type_oid,
    attr_number: column,
    table_oid: table,
    name:,
    ..,
  ) = column

  use type_ <- eval.try(find_gleam_type(query, type_oid))

  let ends_with_exclamation_mark = string.ends_with(name, "!")
  let ends_with_question_mark = string.ends_with(name, "?")
  use nullability <- eval.try(case ends_with_exclamation_mark {
    True -> eval.return(NotNullable)
    False ->
      case ends_with_question_mark {
        True -> eval.return(Nullable)
        False ->
          case set.contains(nullables, i) {
            True -> eval.return(Nullable)
            False -> column_nullability(table:, column:)
          }
      }
  })

  let type_ = case nullability {
    Nullable -> gleam.Option(type_)
    NotNullable -> type_
  }

  let try_convert_name =
    // If the name ends with a `?` or `!` we don't want that to be included in
    // the gleam name or it would be invalid!
    case ends_with_exclamation_mark || ends_with_question_mark {
      True -> string.drop_right(name, 1)
      False -> name
    }
    |> gleam.identifier
    |> result.map_error(invalid_column_error(query, name, _))

  use name <- eval.try(eval.from_result(try_convert_name))

  let field = gleam.Field(label: name, type_:)
  eval.return(field)
}

fn column_nullability(table table: Int, column column: Int) -> Db(Nullability) {
  // We first check if the table+column is cached to avoid making redundant
  // queries to the database.
  use <- with_cached_column(table:, column:)

  // If the table oid is 0 that means the column doesn't come from any table so
  // we just assume it's not nullable.
  use <- bool.guard(when: table == 0, return: eval.return(NotNullable))

  // This query has 2 parameters:
  // - the oid of the table (a 32bit integer, oid is 23)
  // - the index of the column (a 32 bit integer, oid is 23)
  let params = [pg.Parameter(<<table:32>>), pg.Parameter(<<column:32>>)]
  let run_query =
    find_column_nullability_query |> run_query("", params, [23, 23])
  use res <- eval.try(run_query)

  // We know the output will only have only one column, that is the boolean
  // telling us if the column has a not-null constraint.
  let assert [has_non_null_constraint] = res
  case bit_array_to_bool(has_non_null_constraint) {
    True -> eval.return(NotNullable)
    False -> eval.return(Nullable)
  }
}

// --- DB ACTION HELPERS -------------------------------------------------------

/// Runs a query against the database.
/// - `parameters` are the parameter values that need to be supplied in place of
///   the query placeholders
/// - `parameters_object_ids` are the oids describing the type of each
///   parameter.
///
/// > ⚠️ The `parameters_objects_ids` should have the same length of
/// > `parameters` and correctly describe each parameter's type. This function
/// > makes no attempt whatsoever to verify this assumption is correct so be
/// > careful!
///
/// > ⚠️ This function makes the assumption that the query will only return one
/// > single row. This is totally fine here because we only use this to run
/// > specific hard coded queries that are guaranteed to return a single row.
///
fn run_query(
  query: String,
  query_file: String,
  parameters: List(pg.ParameterValue),
  parameters_object_ids: List(Int),
) -> Db(List(BitArray)) {
  // The message exchange to run a query works as follow:
  // - `Parse` we ask the server to parse the query, we do not give it a name
  // - `Bind` we bind the query to the unnamed portal so that it is ready to
  //   be executed.
  // - `Execute` we ask the server to run the unnamed portal and return all
  //   the rows (0 means return all rows).
  // - `Close` we ask to close the unnamed query and the unnamed portal to free
  //   their resources.
  // - `Sync` this acts as a synchronization point that needs to go at the end
  //   of the sequence before the next one.
  //
  // As you can see in the receiving part, each message we send corresponds to a
  // specific answer from the server:
  // - `ParseComplete` the query was parsed correctly
  // - `BindComplete` the query was bound to a portal
  // - `MessageDataRow` the result(s) coming from the query execution
  // - `CommandComplete` when the result coming from the query is over
  // - `CloseComplete` the portal/statement was closed
  // - `ReadyForQuery` final reply to the sync message signalling we can go on
  //   making new requests
  use _ <- eval.try(
    send_all([
      pg.FeParse("", query, parameters_object_ids),
      pg.FeBind(
        portal: "",
        statement_name: "",
        parameter_format: pg.FormatAll(pg.Binary),
        parameters:,
        result_format: pg.FormatAll(pg.Binary),
      ),
      pg.FeExecute("", 0),
      pg.FeClose(pg.PreparedStatement, ""),
      pg.FeClose(pg.Portal, ""),
      pg.FeSync,
    ]),
  )

  use msg <- eval.try(receive())
  let assert pg.BeParseComplete = msg
  use msg <- eval.try(receive())
  let assert pg.BeBindComplete = msg
  use msg <- eval.try(receive())
  use res <- eval.try(expect_data_row(msg, query_file))
  use msg <- eval.try(receive())
  let assert pg.BeCommandComplete(_, _) = msg
  use msg <- eval.try(receive())
  let assert pg.BeCloseComplete = msg
  use msg <- eval.try(receive())
  let assert pg.BeCloseComplete = msg
  use msg <- eval.try(receive())
  let assert pg.BeReadyForQuery(_) = msg
  eval.return(res)
}

/// Receives a message and expects it to be a data row message.
///
fn expect_data_row(
  msg: pg.BackendMessage,
  query_file: String,
) -> Db(List(BitArray)) {
  case msg {
    pg.BeMessageDataRow(res) -> eval.return(res)
    pg.BeErrorResponse(fields) ->
      case fields_to_permission_denied_error(query_file, fields) {
        Ok(error) -> eval.throw(error)
        Error(_) -> panic as string.inspect(msg)
      }
    _ -> panic as string.inspect(msg)
  }
}

fn fields_to_permission_denied_error(
  query_file: String,
  fields: Set(pg.ErrorOrNoticeField),
) -> Result(Error, Nil) {
  let #(code, reason) = {
    use #(code, reason), field <- set.fold(over: fields, from: #(None, None))
    case field {
      pg.Code(code) -> #(Some(code), reason)
      pg.Message(reason) -> #(code, Some(reason))
      _ -> #(code, reason)
    }
  }
  case code, reason {
    Some("42501"), Some(reason) -> Ok(PgPermissionDenied(query_file:, reason:))
    _, _ -> Error(Nil)
  }
}

/// Receive a single message from the database.
///
fn receive() -> Db(pg.BackendMessage) {
  use Context(db:, ..) as context <- eval.from
  case pg.receive(db) {
    Ok(#(db, msg)) -> #(Context(..context, db:), Ok(msg))
    Error(pg.ReadDecodeError(error)) -> #(
      context,
      Error(PgCannotDecodeReceivedMessage(string.inspect(error))),
    )
    Error(pg.SocketError(error)) -> #(
      context,
      Error(PgCannotReceiveMessage(string.inspect(error))),
    )
  }
}

/// Send a single message to the database.
///
fn send(message message: pg.FrontendMessage) -> Db(Nil) {
  use Context(db:, ..) as context <- eval.from

  let result =
    message
    |> pg.encode_frontend_message
    |> pg.send(db, _)

  let #(db, result) = case result {
    Ok(db) -> #(db, Ok(Nil))
    Error(error) -> #(db, Error(PgCannotSendMessage(string.inspect(error))))
  }

  #(Context(..context, db:), result)
}

/// Send many messages, one after the other.
///
fn send_all(messages messages: List(pg.FrontendMessage)) -> Db(Nil) {
  use acc, msg <- eval_extra.try_fold(messages, from: Nil)
  use _ <- eval.try(send(msg))
  eval.return(acc)
}

/// Start receiving and discarding messages until a `ReadyForQuery` message is
/// received.
///
fn wait_until_ready() -> Db(Nil) {
  use _ <- eval.try(send(pg.FeFlush))
  do_wait_until_ready()
}

fn do_wait_until_ready() -> Db(Nil) {
  use msg <- eval.try(receive())
  case msg {
    pg.BeReadyForQuery(_) -> eval.return(Nil)
    _ -> do_wait_until_ready()
  }
}

/// Throws an error built from an expected message and an unexpected message
/// that is turned into a string.
///
fn unexpected_message(
  builder: fn(String, String) -> Error,
  expected expected: String,
  got got: pg.BackendMessage,
) {
  builder(expected, string.inspect(got)) |> eval.throw
}

/// Throws a `PgUnexpectedAuthentication` error.
///
fn unsupported_authentication(auth: String) {
  eval.throw(PgUnsupportedAuthentication(auth))
}

/// Looks up for a type with the given id in a global cache.
/// If the type is present it immediately returns it.
/// Otherwise it runs the database action to fetch it and then caches it to be
/// reused later.
///
fn with_cached_gleam_type(
  lookup oid: Int,
  otherwise do: fn() -> Db(gleam.Type),
) -> Db(gleam.Type) {
  use context: Context <- eval.from
  case dict.get(context.gleam_types, oid) {
    Ok(type_) -> #(context, Ok(type_))
    Error(_) ->
      case eval.step(do(), context) {
        #(_, Error(_)) as result -> result
        #(Context(gleam_types:, ..) as context, Ok(type_)) -> {
          let gleam_types = dict.insert(gleam_types, oid, type_)
          let new_context = Context(..context, gleam_types:)
          #(new_context, Ok(type_))
        }
      }
  }
}

/// Looks up for the nullability of table's column.
/// If the nullability is cached it is immediately returns it.
/// Otherwise it runs the database action to fetch it and then caches it to be
/// reused later.
///
fn with_cached_column(
  table table_oid: Int,
  column column: Int,
  otherwise do: fn() -> Db(Nullability),
) -> Db(Nullability) {
  use context: Context <- eval.from
  let key = #(table_oid, column)
  case dict.get(context.column_nullability, key) {
    Ok(type_) -> #(context, Ok(type_))
    Error(_) ->
      case eval.step(do(), context) {
        #(_, Error(_)) as result -> result
        #(Context(column_nullability:, ..) as context, Ok(type_)) -> {
          let column_nullability = dict.insert(column_nullability, key, type_)
          let new_context = Context(..context, column_nullability:)
          #(new_context, Ok(type_))
        }
      }
  }
}

// --- HELPERS TO BUILD ERRORS -------------------------------------------------

fn unsupported_type_error(query: UntypedQuery, type_: String) -> Error {
  let UntypedQuery(content:, file:, name:, starting_line:, comment: _) = query
  QueryHasUnsupportedType(
    file:,
    name: gleam.identifier_to_string(name),
    content:,
    type_:,
    starting_line:,
  )
}

fn cannot_parse_error(
  query: UntypedQuery,
  error_code error_code: Option(String),
  hint hint: Option(String),
  additional_error_message additional_error_message: Option(String),
  pointer pointer: Option(Pointer),
) -> Error {
  let UntypedQuery(content:, file:, name:, starting_line:, comment: _) = query
  CannotParseQuery(
    content:,
    file:,
    name: gleam.identifier_to_string(name),
    error_code:,
    hint:,
    pointer:,
    additional_error_message:,
    starting_line:,
  )
}

fn invalid_column_error(
  query: UntypedQuery,
  column_name: String,
  reason: ValueIdentifierError,
) -> Error {
  let UntypedQuery(name: _, file:, content:, starting_line:, comment: _) = query
  QueryHasInvalidColumn(
    file:,
    column_name:,
    suggested_name: gleam.similar_identifier_string(column_name)
      |> option.from_result,
    content:,
    reason:,
    starting_line:,
  )
}

// --- DECODERS ----------------------------------------------------------------

fn json_plans_decoder(data: Dynamic) -> Result(List(Plan), DecodeErrors) {
  d.list(d.field("Plan", plan_decoder))(data)
}

fn plan_decoder(data: Dynamic) -> Result(Plan, DecodeErrors) {
  d.decode4(
    Plan,
    d.optional_field("Join Type", join_type_decoder),
    d.optional_field("Parent Relationship", parent_relation_decoder),
    d.optional_field("Output", d.list(d.string)),
    d.optional_field("Plans", d.list(plan_decoder)),
  )(data)
}

fn join_type_decoder(data: Dynamic) -> Result(JoinType, DecodeErrors) {
  use data <- result.map(d.string(data))
  case data {
    "Full" -> Full
    "Left" -> Left
    "Right" -> Right
    _ -> Other
  }
}

fn parent_relation_decoder(
  data: Dynamic,
) -> Result(ParentRelation, DecodeErrors) {
  use data <- result.map(d.string(data))
  case data {
    "Inner" -> Inner
    _ -> NotInner
  }
}

// --- UTILS -------------------------------------------------------------------

/// Turns a bit array into a boolean value.
/// Returns `False` if the bit array is all `0`s or empty, `True` otherwise.
///
fn bit_array_to_bool(bit_array: BitArray) -> Bool {
  case bit_array {
    <<0, rest:bits>> -> bit_array_to_bool(rest)
    <<>> -> False
    _ -> True
  }
}
