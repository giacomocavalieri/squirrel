import gleam/list
import gleam/string
import justin
import squirrel/internal/error.{
  type ValueIdentifierError, ContainsInvalidGrapheme, IsEmpty,
}

/// A Gleam type.
/// This could also be a custom named type coming from other packages (like
/// `Json` or `Uuid`), it doesn't have to be limited to built-in types.
///
pub type Type {
  List(Type)
  Option(Type)
  Date
  Timestamp
  BitArray
  Int
  Float
  Bool
  String
  Json
  Uuid
}

/// The labelled field of a Gleam record.
///
pub type Field {
  Field(label: ValueIdentifier, type_: Type)
}

/// A Gleam identifier, that is a string that starts with a lowercase letter,
/// is in snake_case and can only contain lowercase letters, numbers and
/// underscores.
///
/// > 💡 This can only be built using the `gleam.identifier` function that
/// > ensures that a string is a valid Gleam identifier.
///
pub opaque type ValueIdentifier {
  ValueIdentifier(String)
}

/// Returns true if the given string is a valid Gleam identifier (that is not
/// a discard identifier, that is starting with an '_').
///
/// > 💡 A valid identifier can be described by the following regex:
/// > `[a-z][a-z0-9_]*`.
pub fn identifier(
  from name: String,
) -> Result(ValueIdentifier, ValueIdentifierError) {
  // A valid identifier needs to start with a lowercase letter.
  // We do not accept _discard identifier as valid.
  case name {
    "a" <> rest
    | "b" <> rest
    | "c" <> rest
    | "d" <> rest
    | "e" <> rest
    | "f" <> rest
    | "g" <> rest
    | "h" <> rest
    | "i" <> rest
    | "j" <> rest
    | "k" <> rest
    | "l" <> rest
    | "m" <> rest
    | "n" <> rest
    | "o" <> rest
    | "p" <> rest
    | "q" <> rest
    | "r" <> rest
    | "s" <> rest
    | "t" <> rest
    | "u" <> rest
    | "v" <> rest
    | "w" <> rest
    | "x" <> rest
    | "y" <> rest
    | "z" <> rest -> to_identifier_rest(name, rest, 1)
    _ ->
      case string.pop_grapheme(name) {
        Ok(#(g, _)) -> Error(ContainsInvalidGrapheme(0, g))
        Error(_) -> Error(IsEmpty)
      }
  }
}

fn to_identifier_rest(
  name: String,
  rest: String,
  position: Int,
) -> Result(ValueIdentifier, ValueIdentifierError) {
  // The rest of an identifier can only contain lowercase letters, _, numbers,
  // or be empty. In all other cases it's not valid.
  case rest {
    "a" <> rest
    | "b" <> rest
    | "c" <> rest
    | "d" <> rest
    | "e" <> rest
    | "f" <> rest
    | "g" <> rest
    | "h" <> rest
    | "i" <> rest
    | "j" <> rest
    | "k" <> rest
    | "l" <> rest
    | "m" <> rest
    | "n" <> rest
    | "o" <> rest
    | "p" <> rest
    | "q" <> rest
    | "r" <> rest
    | "s" <> rest
    | "t" <> rest
    | "u" <> rest
    | "v" <> rest
    | "w" <> rest
    | "x" <> rest
    | "y" <> rest
    | "z" <> rest
    | "_" <> rest
    | "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest -> to_identifier_rest(name, rest, position + 1)
    "" -> Ok(ValueIdentifier(name))
    _ ->
      case string.pop_grapheme(rest) {
        Ok(#(g, _)) -> Error(ContainsInvalidGrapheme(position, g))
        Error(_) -> panic as "unreachable: empty identifier rest should be ok"
      }
  }
}

/// Turns an identifier back into a String.
///
pub fn identifier_to_string(identifier: ValueIdentifier) -> String {
  let ValueIdentifier(name) = identifier
  name
}

/// Turns a Gleam identifier into a type name. That is it strips it of all its
/// underscores and makes it PascalCase.
///
pub fn identifier_to_type_name(identifier: ValueIdentifier) -> String {
  let ValueIdentifier(name) = identifier

  justin.pascal_case(name)
  |> string.to_graphemes
  // We want to remove any leftover "_" that might still be present after the
  // conversion if the identifier had consecutive "_".
  |> list.filter(keeping: fn(c) { c != "_" })
  |> string.join(with: "")
}

/// Tries to suggest a valid Gleam identifier as similar as possible to a given
/// String.
///
/// If it cannot come up with a suggestion, it returns `Error(Nil)`.
///
pub fn similar_identifier_string(string: String) -> Result(String, Nil) {
  let proposal =
    string.trim(string)
    |> justin.snake_case
    |> string.to_graphemes
    |> list.drop_while(fn(g) { g == "_" || is_digit(g) })
    |> list.filter(keeping: is_identifier_char)
    |> string.join(with: "")

  case proposal {
    "" -> Error(Nil)
    _ -> Ok(proposal)
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_identifier_char(char: String) -> Bool {
  case char {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "_"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9" -> True
    _ -> False
  }
}
