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
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import squirrel/internal/database/postgres_protocol as pg
import squirrel/internal/error.{
  type EnumError, type Error, type Pointer, type ValueIdentifierError, ByteIndex,
  Pointer,
}

import squirrel/internal/eval_extra
import squirrel/internal/gleam
import squirrel/internal/query.{type TypedQuery, type UntypedQuery, UntypedQuery}
import squirrel/internal/scram

const minimum_required_version = 160_000

fn find_postgres_type_query() -> UntypedQuery {
  let assert Ok(name) = gleam.value_identifier("find_postgres_type_query")

  query.UntypedQuery(
    file: "",
    starting_line: 1,
    name:,
    comment: [],
    content: "
select
  -- The name of the type or, if the type is an array, the name of its
  -- elements' type.
  case
    when elem.typname is null then type.typname
    else elem.typname
 	end as type,

  -- The oid of the type or the array item type.
  case
    when elem.typname is null then type.oid
    else elem.oid
 	end as oid,

  -- Tells us how to interpret the first column: if this is true then the first
  -- column is the type of the elements of the array type.
  -- Otherwise it means we've found a base type.
  case
    when elem.typname is null then false
    else true
 	end as is_array,

  -- The type of the type/array item.
  -- It will be 'e' if the thing is an enum.
  case
    when elem.typname is null then type.typtype
    else elem.typtype
  end as kind
from
  pg_type as type
  left join pg_type as elem on type.typelem = elem.oid
where
  type.oid = $1
",
  )
}

fn find_enum_variants_query() -> UntypedQuery {
  let assert Ok(name) = gleam.value_identifier("find_enum_variants_query")

  query.UntypedQuery(
    file: "",
    starting_line: 1,
    name:,
    comment: [],
    content: "
select
	enumlabel
from
	pg_enum
where
	enumtypid = $1
order by
	enumsortorder asc
",
  )
}

fn find_column_nullability_query() -> UntypedQuery {
  let assert Ok(name) = gleam.value_identifier("find_column_nullability_query")

  query.UntypedQuery(
    file: "",
    starting_line: 1,
    name:,
    comment: [],
    content: "
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
",
  )
}

fn find_postgres_version_query() -> UntypedQuery {
  let assert Ok(name) = gleam.value_identifier("find_postgres_version_query")

  query.UntypedQuery(
    file: "",
    starting_line: 1,
    name:,
    comment: [],
    content: "select current_setting('server_version_num')",
  )
}

// --- TYPES -------------------------------------------------------------------

/// A Postgres type.
///
/// > ⚠️ Postgres has loads of types and this might not cover the more exotic
/// > ones but for now it feels more than enough.
///
type PgType {
  /// A base type, like `integer`, `text`, `char`, ...
  ///
  PBase(name: String)

  /// An array type like `int[]`, `text[]`, ...
  ///
  PArray(inner: PgType)

  /// An enum, for example:
  ///
  /// ```sql
  /// create type squirrel_colour as enum(
  ///   'red',
  ///   'grey',
  ///   'light brown'
  /// )
  /// ```
  ///
  ///
  PEnum(name: String, variants: List(String))

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
  Plan(join_type: Option(JoinType), output: List(String), plans: List(Plan))
}

type JoinType {
  FullJoin
  LeftJoin
  RightJoin
  InnerJoin
  /// > ⚠️ From my understanding a semi join only returns rows from the relation
  /// > on the left. It also has some other invariants regarding duplicates that
  /// > are not really relevant to squirrel.
  /// > It looks like this is virtually the same as a left join as far as code
  /// > generation is concerned!
  ///
  SemiJoin
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
fn pg_to_gleam_type(
  query: UntypedQuery,
  type_: PgType,
) -> Result(gleam.Type, Error) {
  case type_ {
    PArray(inner:) ->
      pg_to_gleam_type(query, inner)
      |> result.map(gleam.List)

    POption(inner:) ->
      pg_to_gleam_type(query, inner)
      |> result.map(gleam.Option)

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
        _ -> Error(unsupported_type_error(query, name))
      }

    PEnum(name:, variants:) ->
      gleam.try_make_enum(name, variants)
      |> result.map_error(invalid_enum_error(query, name, _))
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
    |> result.map_error(error.PgCannotEstablishTcpConnection(
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

  let setup_script = {
    use _ <- eval.try(authenticate(connection))
    use _ <- eval.try(ensure_postgres_version())
    eval.return(Nil)
  }

  let #(context, connection) = eval.step(setup_script, context)
  use _ <- result.try(connection)

  // After successfully authenticating we can try and type all the queries.
  list.map(queries, infer_types)
  |> eval_extra.run_all(context)
  |> result.partition
  |> Ok
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
    _ ->
      unexpected_message(error.PgUnexpectedAuthMethodMessage, "AuthMethod", msg)
  })

  use _ <- eval.try(
    wait_until_ready()
    // In case there's a receive error while waiting for the server to be ready
    // we want to display a more helpful error message because the problem here
    // must be with an invalid username/database combination.
    |> eval.replace_error(error.PgInvalidUserDatabase(
      user: connection.user,
      database: connection.database,
    )),
  )

  eval.return(Nil)
}

/// Makes sure that the Postgres version is supported by Squirrel
fn ensure_postgres_version() -> Db(Nil) {
  use version <- eval.try(run_query(find_postgres_version_query(), [], []))
  let assert [[version, ..], ..] = version
    as "select version should always return at least one row"

  case bit_array.to_string(version) |> result.then(int.parse) {
    Error(_) -> eval.throw(error.PostgresVersionHasInvalidFormat(version))
    Ok(version) if version >= minimum_required_version -> eval.return(Nil)
    Ok(_) -> eval.throw(error.PostgresVersionIsTooOld)
  }
}

fn cleartext_authenticate(user: String, password: String) -> Db(Nil) {
  use _ <- eval.try(send(pg.FeAmbigous(pg.FePasswordMessage(password))))
  use msg <- eval.try(receive())
  case msg {
    pg.BeAuthenticationOk -> eval.return(Nil)
    pg.BeErrorResponse(_) -> eval.throw(error.PgInvalidPassword(user:))

    // The response should only ever be Ok or Error (in case the password is
    // not correct).
    _ ->
      unexpected_message(
        error.PgUnexpectedCleartextAuthMessage,
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
        error.PgUnexpectedSha256AuthMessage,
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
      error.PgUnexpectedSha256AuthMessage,
      "AuthenticationSASLFinal or BeErrorResponse",
      msg,
    )

  case msg {
    pg.BeAuthenticationSASLFinal(msg) -> eval.return(msg)
    pg.BeErrorResponse(fields) ->
      case set.contains(fields, pg.Code("28P01")) {
        True -> eval.throw(error.PgInvalidPassword(user))
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
    Error(Nil) -> eval.throw(error.PgInvalidSha256ServerProof)
    Ok(scram.Failed(_)) -> eval.throw(error.PgInvalidPassword(user))
    Ok(scram.Successful(server_proof)) ->
      case server_proof == expected_server_proof {
        True -> eval.return(Nil)
        False -> eval.throw(error.PgInvalidSha256ServerProof)
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
  use #(parameters, returns) <- eval.try(parameters_and_returns(query))
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
  use plan <- eval.try(query_plan(query))
  let nullables = nullables_from_plan(plan)
  use returns <- eval.try(resolve_returns(query, returns, nullables))

  query
  |> query.add_types(parameters, returns)
  |> eval.from_result
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
    error.PgCannotDescribeQuery(
      file: query.file,
      query_name: gleam.value_identifier_to_string(query.name),
      expected:,
      got:,
    )
  }

  use msg <- eval.try(receive())
  case msg {
    pg.BeErrorResponse(errors) -> {
      // In case we get an error response back from the server at this stage the
      // server will also send a `ReadyForQuery` after it to signal it's ready
      // to keep going with other queries.
      // So we expect to see that.
      use msg <- eval.try(receive())
      case msg {
        pg.BeReadyForQuery(_) ->
          eval.throw(error_fields_to_parse_error(query, errors))
        _ -> unexpected_message(cannot_describe, "BeReadyForQuery(_)", msg)
      }
    }
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
  use res <- eval.try(run_query(find_postgres_type_query(), params, [23]))

  // We know the output must only contain four values: the name, the oid, a boolean to
  // check wether it is an array or not and the type of the type / array item.
  // It's safe to assert because this query is hard coded in our code and the
  // output shape cannot change without us changing that query.
  let assert [[name, oid, is_array, kind]] = res
  let assert Ok(name) = bit_array.to_string(name)
  let assert Ok(kind) = bit_array.to_string(kind)
  let assert <<oid:size(32)>> = oid

  use type_ <- eval.try(case kind {
    "e" -> resolve_enum_type(name, oid)
    _ -> eval.return(PBase(name))
  })

  // We then decode the bitarrays we got as a result:
  // - `name` is just a string
  // - `is_array` is a pg boolean
  //
  let type_ = case bit_array_to_bool(is_array) {
    True -> PArray(type_)
    False -> type_
  }

  pg_to_gleam_type(query, type_)
  |> eval.from_result
}

fn resolve_enum_type(name: String, oid: Int) -> Db(PgType) {
  let params = [pg.Parameter(<<oid:32>>)]
  use rows <- eval.try(run_query(find_enum_variants_query(), params, [23]))
  let variants =
    list.map(rows, fn(row) {
      // We get multiple rows from running the query, each is a single string
      // that is a variant name.
      let assert [variant] = row
      let assert Ok(variant) = bit_array.to_string(variant)
      variant
    })

  eval.return(PEnum(name:, variants:))
}

/// Returns the query plan for a given query.
/// `parameters` is the number of parameter placeholders in the query.
///
fn query_plan(query: UntypedQuery) -> Db(Plan) {
  use plan <- eval.try(run_explain_query(query))

  // We know the output will only contain a single row that is the json string
  // containing the query plan.
  case json.parse_bits(plan, json_plans_decoder()) {
    Ok([plan, ..]) -> eval.return(plan)
    Ok([]) -> panic as "unreachable: no query plan"
    Error(reason) ->
      eval.throw(error.CannotParsePlanForQuery(file: query.file, reason:))
  }
}

/// > 🚨 This is safe to run _only if it's after having prepared the query
/// > earlier!_
/// > This is because we're running the explain query in the simple mode that
/// > allows running an arbitrary number of sql statements. So if your sql file
/// > contains more than a query like this one:
/// >
/// > ```sql
/// > select 1;
/// > drop table users;
/// > ```
/// >
/// > What you'd end up running would look like this:
/// > ```gleam
/// > "explain (verbose, generic_plan) select 1;
/// > drop table users;"
/// > ```
/// >
/// > Meaning it would erase your db!
///
fn run_explain_query(query: UntypedQuery) -> Db(BitArray) {
  let explain_query =
    "explain (format json, verbose, generic_plan) " <> query.content

  use _ <- eval.try(send_all([pg.FeQuery(explain_query)]))

  use msg <- eval.try(receive())
  use _ <- eval.try(expect_query_plan_row_description(msg, query))
  use msg <- eval.try(receive())
  let assert pg.BeMessageDataRow([query_plan]) = msg
  use msg <- eval.try(receive())
  let assert pg.BeCommandComplete(_, _) = msg
  use msg <- eval.try(receive())
  let assert pg.BeReadyForQuery(_) = msg
  eval.return(query_plan)
}

/// Given a query plan, returns a set with the indices of the output columns
/// that can contain null values.
///
fn nullables_from_plan(plan: Plan) -> Set(Int) {
  let outputs = list.index_fold(plan.output, dict.new(), dict.insert)
  do_nullables_from_plan(plan, outputs, set.new())
}

fn do_nullables_from_plan(
  plan: Plan,
  // A dict from "column name" to its position in the query output.
  query_outputs: Dict(String, Int),
  nullables: Set(Int),
) -> Set(Int) {
  case plan.join_type, plan.plans {
    // If this is a full join then all its outputs could be optional!!
    Some(FullJoin), _ ->
      plan_outputs_indices(plan, query_outputs)
      |> set.union(nullables)

    // If this is a right join then we must mark the outputs of its left part as
    // nullable!
    Some(RightJoin), [left, right] -> {
      let nullables =
        plan_outputs_indices(left, query_outputs)
        |> set.union(nullables)

      do_nullables_from_plan(right, query_outputs, nullables)
    }

    // If this is a left join then we must mark the outputs of its right part as
    // nullable!
    Some(LeftJoin), [left, right] | Some(SemiJoin), [left, right] -> {
      let nullables =
        plan_outputs_indices(right, query_outputs)
        |> set.union(nullables)

      do_nullables_from_plan(left, query_outputs, nullables)
    }

    // This should never happen in theory (a join with 0, 1, or more than two
    // childs), so we just inspect their plans as a safe bet.
    Some(RightJoin), plans
    | Some(LeftJoin), plans
    | Some(SemiJoin), plans
    | None, plans
    -> {
      use nullables, plan <- list.fold(plans, nullables)
      do_nullables_from_plan(plan, query_outputs, nullables)
    }

    // If this is an inner join then it's outputs are not necessarily nullable,
    // we inspect the children's plans to see if they do have some nullable
    // columns.
    Some(InnerJoin), plans -> {
      use nullables, plan <- list.fold(plans, nullables)
      do_nullables_from_plan(plan, query_outputs, nullables)
    }
  }
}

fn plan_outputs_indices(
  plan: Plan,
  query_outputs: Dict(String, Int),
) -> Set(Int) {
  use nullables, output <- list.fold(plan.output, from: set.new())
  case dict.get(query_outputs, output) {
    Ok(i) -> set.insert(nullables, i)
    Error(_) -> nullables
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
      True -> string.drop_end(name, 1)
      False -> name
    }
    |> gleam.value_identifier
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
  let run_query = run_query(find_column_nullability_query(), params, [23, 23])
  use res <- eval.try(run_query)

  // We know the output will only have only one column, that is the boolean
  // telling us if the column has a not-null constraint.
  let assert [[has_non_null_constraint]] = res
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
  query: UntypedQuery,
  parameters: List(pg.ParameterValue),
  parameters_object_ids: List(Int),
) -> Db(List(List(BitArray))) {
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
      pg.FeParse("", query.content, parameters_object_ids),
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
  use _ <- eval.try(expect_parse_complete(msg, query))
  use msg <- eval.try(receive())
  let assert pg.BeBindComplete = msg
  use res <- eval.try(accumulate_data_rows_until_command_complete(query.file))
  use msg <- eval.try(receive())
  let assert pg.BeCloseComplete = msg
  use msg <- eval.try(receive())
  let assert pg.BeCloseComplete = msg
  use msg <- eval.try(receive())
  let assert pg.BeReadyForQuery(_) = msg
  eval.return(res)
}

/// Receives and consumes `DataRow` messages until a `CommandComplete` message
/// is received. All the data transmitted through the data rows is then
/// returned.
///
fn accumulate_data_rows_until_command_complete(query_file: String) {
  do_accumulate_data_rows_until_command_complete(query_file, [])
}

fn do_accumulate_data_rows_until_command_complete(
  query_file: String,
  acc: List(List(BitArray)),
) -> _ {
  use msg <- eval.try(receive())
  case msg {
    pg.BeCommandComplete(_, _) -> eval.return(list.reverse(acc))
    pg.BeMessageDataRow(data) ->
      do_accumulate_data_rows_until_command_complete(query_file, [data, ..acc])
    pg.BeErrorResponse(fields) ->
      case fields_to_permission_denied_error(query_file, fields) {
        Ok(error) -> eval.throw(error)
        Error(_) -> panic as string.inspect(msg)
      }
    _ -> panic as string.inspect(msg)
  }
}

fn expect_parse_complete(msg: pg.BackendMessage, query: UntypedQuery) -> Db(Nil) {
  case msg {
    pg.BeParseComplete -> eval.return(Nil)

    pg.BeErrorResponse(fields) -> {
      let UntypedQuery(file:, name:, ..) = query
      let query_name = gleam.value_identifier_to_string(name)
      let unexpected = fn(expected, got) {
        error.PgCannotDescribeQuery(file:, query_name:, expected:, got:)
      }
      let parse_error = error_fields_to_parse_error(query, fields)
      expect_ready_for_query_then_throw(parse_error, or: unexpected)
    }

    _ -> panic as string.inspect(msg)
  }
}

fn expect_query_plan_row_description(
  msg: pg.BackendMessage,
  query: UntypedQuery,
) -> Db(Nil) {
  case msg {
    pg.BeRowDescriptions([pg.RowDescriptionField(name: "QUERY PLAN", ..)]) ->
      eval.return(Nil)

    pg.BeErrorResponse(fields) -> {
      let UntypedQuery(file:, name:, ..) = query
      let query_name = gleam.value_identifier_to_string(name)
      let unexpected = fn(expected, got) {
        error.PgCannotExplainQuery(file:, query_name:, expected:, got:)
      }

      let parse_error =
        error_fields_to_parse_error(query, fields)
        |> adjust_parse_error_for_explain

      expect_ready_for_query_then_throw(parse_error, or: unexpected)
    }

    _ -> panic as string.inspect(msg)
  }
}

fn expect_ready_for_query_then_throw(error, or to_error) {
  use msg <- eval.try(receive())
  case msg {
    pg.BeReadyForQuery(_) -> eval.throw(error)
    _ -> unexpected_message(to_error, "BeReadyForQuery(_)", msg)
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
    Some("42501"), Some(reason) ->
      Ok(error.PgPermissionDenied(query_file:, reason:))
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
      Error(error.PgCannotDecodeReceivedMessage(string.inspect(error))),
    )
    Error(pg.SocketError(error)) -> #(
      context,
      Error(error.PgCannotReceiveMessage(string.inspect(error))),
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
    Error(error) -> #(
      db,
      Error(error.PgCannotSendMessage(string.inspect(error))),
    )
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
fn unsupported_authentication(auth: String) -> Db(a) {
  eval.throw(error.PgUnsupportedAuthentication(auth))
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
  error.QueryHasUnsupportedType(
    file:,
    name: gleam.value_identifier_to_string(name),
    content:,
    type_:,
    starting_line:,
  )
}

fn invalid_enum_error(query: UntypedQuery, enum_name: String, reason: EnumError) {
  let UntypedQuery(content:, file:, name: _, starting_line:, comment: _) = query
  error.QueryHasInvalidEnum(
    file:,
    content:,
    starting_line:,
    enum_name:,
    reason:,
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
  error.CannotParseQuery(
    content:,
    file:,
    name: gleam.value_identifier_to_string(name),
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
  error.QueryHasInvalidColumn(
    file:,
    column_name:,
    suggested_name: gleam.similar_value_identifier_string(column_name)
      |> option.from_result,
    content:,
    reason:,
    starting_line:,
  )
}

/// Some queries might be parsed correctly at first but result in a parse error
/// later when we try to `explain` those. For example:
///
/// ```sql
/// ; select 1;
/// ```
///
/// Will parse fine at first but when we try to explain it we get a parse error
/// since the query would be:
///
/// ```sql
/// explain (format json, verbose, generic_plan) ; select 1;
/// --                                           ^ parse error here
/// ```
///
/// So in case we get a parse error during the explain step we want to slightly
/// change it: we drop the explain part from the query so it doesn't show up in
/// the public facing error.
///
fn adjust_parse_error_for_explain(error: Error) -> Error {
  case error {
    error.CannotParseQuery(pointer:, ..) -> {
      let pointer = case pointer {
        Some(Pointer(ByteIndex(index), message)) ->
          // We also need to update any pointer to make sure it's pointing to
          // the right place since we've dropped the first 46 bytes of the
          // query's content.
          Some(Pointer(ByteIndex(index - 45), message))
        _ -> pointer
      }
      error.CannotParseQuery(..error, pointer:)
    }

    _ -> error
  }
}

// --- DECODERS ----------------------------------------------------------------

fn json_plans_decoder() -> Decoder(List(Plan)) {
  decode.list(decode.at(["Plan"], plan_decoder()))
}

fn plan_decoder() -> Decoder(Plan) {
  use join_type <- decode.optional_field("Join Type", None, join_type_decoder())
  use output <- decode.optional_field("Output", [], decode.list(decode.string))
  use plans <- decode.optional_field("Plans", [], decode.list(plan_decoder()))
  decode.success(Plan(join_type:, output:, plans:))
}

fn join_type_decoder() -> Decoder(Option(JoinType)) {
  use data <- decode.then(decode.string)
  case data {
    "Full" -> decode.success(Some(FullJoin))
    "Left" -> decode.success(Some(LeftJoin))
    "Right" -> decode.success(Some(RightJoin))
    "Inner" -> decode.success(Some(InnerJoin))
    "Semi" -> decode.success(Some(SemiJoin))
    _ -> decode.failure(None, "JoinType")
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
