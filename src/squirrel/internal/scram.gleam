//// This module is a mostly faithful translation of
//// https://github.com/erleans/pgo/blob/main/src/pgo_scram.erl
////
//// It might not be as well commented as the other ones since some details are
//// of the protocol are not completely clear to me.
////

import gleam/bit_array
import gleam/crypto
import gleam/int
import gleam/result
import gleam/string

/// The first message sent by the client to initiate the authentication message
/// exchange.
///
pub type ClientFirst {
  ClientFirst(user: String, nonce: String)
}

/// The final message sent by the client.
///
pub type ClientLast {
  ClientLast(
    client_first: ClientFirst,
    server_first: ServerFirst,
    password: String,
  )
}

/// The server's response to the client's first message.
///
pub type ServerFirst {
  ServerFirst(nonce: String, salt: BitArray, iterations: Int, raw: BitArray)
}

pub type ServerLast {
  Successful(server_proof: BitArray)
  Failed(error: BitArray)
}

/// Generates a nonce that is guaranteed to be unique and have 16 bytes of
/// random data that can be used for the SCRAM protocol.
///
pub fn nonce() -> String {
  let random_bytes = 16
  let bytes = crypto.strong_random_bytes(random_bytes)
  let unique = encode_unsigned(unique())
  let nonce = <<random_bytes, bytes:bits-size(random_bytes), unique:bits>>
  bit_array.base64_encode(nonce, True)
}

pub fn encode_client_first(msg: ClientFirst) -> BitArray {
  <<"n,,":utf8, client_first_without_header(msg):bits>>
}

fn client_first_without_header(msg: ClientFirst) -> BitArray {
  <<"n=":utf8, msg.user:utf8, ",r=":utf8, msg.nonce:utf8>>
}

pub fn encode_client_last(client_last: ClientLast) -> #(BitArray, BitArray) {
  let ClientLast(
    client_first: client_first,
    server_first: server_first,
    password: password,
  ) = client_last

  // Here we hard coded the SHA256 algorithm since it's the one used by
  // Postgres, but it could be anything really.
  let alg = crypto.Sha256

  let client_final_without_proof = <<
    // No channel binding is implemented.
    "c=biws,":utf8,
    "r=":utf8,
    server_first.nonce:utf8,
  >>

  let auth = <<
    client_first_without_header(client_first):bits,
    ",":utf8,
    server_first.raw:bits,
    ",":utf8,
    client_final_without_proof:bits,
  >>

  let salted_password =
    hi(password, alg, server_first.salt, server_first.iterations)

  let client_key = crypto.hmac(<<"Client Key":utf8>>, alg, salted_password)
  let stored_key = crypto.hash(alg, client_key)
  let client_signature = crypto.hmac(auth, alg, stored_key)
  let client_proof =
    binary_xor(client_key, client_signature)
    |> bit_array.base64_encode(True)

  let server_key = crypto.hmac(<<"Server Key":utf8>>, alg, salted_password)
  let server_signature = crypto.hmac(auth, alg, server_key)
  let client_last = <<
    client_final_without_proof:bits,
    ",p=":utf8,
    client_proof:utf8,
  >>

  #(client_last, server_signature)
}

fn hi(string, algorithm, salt, iterations) {
  let acc =
    crypto.hmac(<<salt:bits, 1:int-big-size(32)>>, algorithm, <<string:utf8>>)
  do_hi(<<string:utf8>>, algorithm, acc, acc, iterations - 1)
}

fn do_hi(string, algorithm, u, hi, iterations) {
  case iterations <= 0 {
    True -> hi
    False -> {
      let u = crypto.hmac(u, algorithm, string)
      let hi = binary_xor(hi, u)
      do_hi(string, algorithm, u, hi, iterations - 1)
    }
  }
}

/// Parses a `ServerFirst` message exchanged during the SCRAM protocol initiated
/// by a Postgres server.
///
pub fn parse_server_first(
  msg: BitArray,
  client_nonce: String,
) -> Result(ServerFirst, Nil) {
  use string_msg <- result.try(bit_array.to_string(msg))
  let parts = string.split(string_msg, on: ",")

  case parts {
    ["r=" <> nonce, "s=" <> salt, "i=" <> iterations]
    | ["r=" <> nonce, "i=" <> iterations, "s=" <> salt]
    | ["i=" <> iterations, "s=" <> salt, "r=" <> nonce]
    | ["i=" <> iterations, "r=" <> nonce, "s=" <> salt]
    | ["s=" <> salt, "r=" <> nonce, "i=" <> iterations]
    | ["s=" <> salt, "i=" <> iterations, "r=" <> nonce] -> {
      use iterations <- result.try(int.parse(iterations))
      use salt <- result.try(bit_array.base64_decode(salt))
      // We've got to make sure the server and client nonce are the same
      // (it is enough for the client_nonce to be a prefix of the server one).
      case string.starts_with(nonce, client_nonce) {
        True ->
          Ok(ServerFirst(
            nonce: nonce,
            iterations: iterations,
            salt: salt,
            raw: msg,
          ))
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

pub fn parse_server_final(msg: BitArray) -> Result(ServerLast, Nil) {
  case msg {
    <<"v=":utf8, rest:bits>> ->
      case binary_split(rest, <<",":utf8>>) {
        [proof, ..] -> {
          use proof <- result.try(bit_array.to_string(proof))
          use proof <- result.try(bit_array.base64_decode(proof))
          Ok(Successful(proof))
        }
        _ -> Error(Nil)
      }
    <<"e=":utf8, rest:bits>> -> Ok(Failed(rest))
    _ -> Error(Nil)
  }
}

// --- EXTERNALS ---------------------------------------------------------------

/// A positive integer number guaranteed to be unique, generated with
/// `erlang:unique_integer`.
///
@external(erlang, "squirrel_ffi", "unique")
fn unique() -> Int

@external(erlang, "binary", "encode_unsigned")
fn encode_unsigned(number: Int) -> BitArray

@external(erlang, "crypto", "exor")
fn binary_xor(one: BitArray, other: BitArray) -> BitArray

@external(erlang, "binary", "split")
fn binary_split(binary: BitArray, on: BitArray) -> List(BitArray)
