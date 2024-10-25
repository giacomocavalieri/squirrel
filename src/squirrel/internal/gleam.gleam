import gleam/list
import gleam/result
import gleam/string
import justin
import non_empty_list.{type NonEmptyList}
import squirrel/internal/error.{
  type EnumError, type TypeIdentifierError, type ValueIdentifierError,
  EnumWithNoVariants, InvalidEnumName, InvalidEnumVariants,
  TypeContainsInvalidGrapheme, TypeIsEmpty, ValueContainsInvalidGrapheme,
  ValueIsEmpty,
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

  /// A custom type whose variants have no fields. For example:
  ///
  /// ```gleam
  /// pub type SquirrelColours {
  ///   LightBrown
  ///   Grey
  ///   Red
  /// }
  /// ```
  ///
  Enum(
    original_name: String,
    name: TypeIdentifier,
    variants: NonEmptyList(EnumVariant),
  )
}

pub type EnumVariant {
  EnumVariant(name: TypeIdentifier, string_representation: String)
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
/// > 💡 This can only be built using the `gleam.value_identifier` function that
/// > ensures that a string is a valid Gleam identifier.
///
pub opaque type ValueIdentifier {
  ValueIdentifier(String)
}

/// A Gleam type identifier, that is a string that starts with an uppercase
/// letter, is in PascalCase and can only contain lowercase letters, numbers and
/// uppercase letters.
///
/// > 💡 This can only be built using the `gleam.type_identifier` function that
/// > ensures that a string is a valid Gleam type identifier.
///
pub opaque type TypeIdentifier {
  TypeIdentifier(String)
}

/// Validates if the given string is a valid Gleam value identifier (that is not
/// a discard identifier, that is starting with an '_').
///
/// > 💡 A valid identifier can be described by the following regex:
/// > `[a-z][a-z0-9_]*`.
///
pub fn value_identifier(
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
    | "z" <> rest -> to_value_identifier_rest(name, rest, 1)
    _ ->
      case string.pop_grapheme(name) {
        Ok(#(g, _)) -> Error(ValueContainsInvalidGrapheme(0, g))
        Error(_) -> Error(ValueIsEmpty)
      }
  }
}

fn to_value_identifier_rest(
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
    | "9" <> rest -> to_value_identifier_rest(name, rest, position + 1)
    "" -> Ok(ValueIdentifier(name))
    _ ->
      case string.pop_grapheme(rest) {
        Ok(#(g, _)) -> Error(ValueContainsInvalidGrapheme(position, g))
        Error(_) -> panic as "unreachable: empty identifier rest should be ok"
      }
  }
}

/// Turns a value identifier back into a String.
///
pub fn value_identifier_to_string(identifier: ValueIdentifier) -> String {
  let ValueIdentifier(name) = identifier
  name
}

/// Validates if the given string is a valid Gleam type identifier.
///
/// > 💡 A valid type identifier can be described by the following regex:
/// > `[A-Z][A-Za-z0-9]*`.
///
pub fn type_identifier(
  from name: String,
) -> Result(TypeIdentifier, TypeIdentifierError) {
  // A valid type identifier needs to start with an uppercase letter.
  case name {
    "A" <> rest
    | "B" <> rest
    | "C" <> rest
    | "D" <> rest
    | "E" <> rest
    | "F" <> rest
    | "G" <> rest
    | "H" <> rest
    | "I" <> rest
    | "J" <> rest
    | "K" <> rest
    | "L" <> rest
    | "M" <> rest
    | "N" <> rest
    | "O" <> rest
    | "P" <> rest
    | "Q" <> rest
    | "R" <> rest
    | "S" <> rest
    | "T" <> rest
    | "U" <> rest
    | "V" <> rest
    | "W" <> rest
    | "X" <> rest
    | "Y" <> rest
    | "Z" <> rest -> to_type_identifier_rest(name, rest, 1)
    _ ->
      case string.pop_grapheme(name) {
        Ok(#(g, _)) -> Error(TypeContainsInvalidGrapheme(0, g))
        Error(_) -> Error(TypeIsEmpty)
      }
  }
}

fn to_type_identifier_rest(
  name: String,
  rest: String,
  position: Int,
) -> Result(TypeIdentifier, TypeIdentifierError) {
  // The rest of an identifier can only contain lowercase or uppercase letters,
  // numbers, or be empty. In all other cases it's not valid.
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
    | "A" <> rest
    | "B" <> rest
    | "C" <> rest
    | "D" <> rest
    | "E" <> rest
    | "F" <> rest
    | "G" <> rest
    | "H" <> rest
    | "I" <> rest
    | "J" <> rest
    | "K" <> rest
    | "L" <> rest
    | "M" <> rest
    | "N" <> rest
    | "O" <> rest
    | "P" <> rest
    | "Q" <> rest
    | "R" <> rest
    | "S" <> rest
    | "T" <> rest
    | "U" <> rest
    | "V" <> rest
    | "W" <> rest
    | "X" <> rest
    | "Y" <> rest
    | "Z" <> rest
    | "0" <> rest
    | "1" <> rest
    | "2" <> rest
    | "3" <> rest
    | "4" <> rest
    | "5" <> rest
    | "6" <> rest
    | "7" <> rest
    | "8" <> rest
    | "9" <> rest -> to_type_identifier_rest(name, rest, position + 1)
    "" -> Ok(TypeIdentifier(name))
    _ ->
      case string.pop_grapheme(rest) {
        Ok(#(g, _)) -> Error(TypeContainsInvalidGrapheme(position, g))
        Error(_) -> panic as "unreachable: empty identifier rest should be ok"
      }
  }
}

/// Turns a type identifier back into a String.
///
pub fn type_identifier_to_string(identifier: TypeIdentifier) -> String {
  let TypeIdentifier(name) = identifier
  name
}

/// Turns a Gleam value identifier into a type name. That is it strips it of all
/// its underscores and makes it PascalCase.
///
pub fn value_identifier_to_type_identifier(
  identifier: ValueIdentifier,
) -> TypeIdentifier {
  let ValueIdentifier(name) = identifier

  let type_identifier =
    justin.pascal_case(name)
    |> string.to_graphemes
    // We want to remove any leftover "_" that might still be present after the
    // conversion if the identifier had consecutive "_".
    |> list.filter(keeping: fn(c) { c != "_" })
    |> string.join(with: "")

  TypeIdentifier(type_identifier)
}

/// Turns a Gleam type identifier into a value identifier by making it
/// snake_case.
///
pub fn type_identifier_to_value_identifier(
  identifier: TypeIdentifier,
) -> ValueIdentifier {
  let TypeIdentifier(name) = identifier
  ValueIdentifier(justin.snake_case(name))
}

/// Tries to suggest a valid Gleam identifier as similar as possible to a given
/// String.
///
/// If it cannot come up with a suggestion, it returns `Error(Nil)`.
///
pub fn similar_value_identifier_string(string: String) -> Result(String, Nil) {
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

/// Tries to build an enum from the given name and fields list.
/// This will try its best to convert snake_case names into PascalCase names
/// before failing!
///
pub fn try_make_enum(
  raw_name: String,
  variants: List(String),
) -> Result(Type, EnumError) {
  use name <- result.try(
    // We first try converting the name to pascal case since SQL's standard is
    // to use snake_case for types and we don't want to fail for that.
    justin.pascal_case(raw_name)
    |> type_identifier
    |> result.replace_error(InvalidEnumName(raw_name)),
  )

  let #(variants, errors) =
    result.partition({
      use variant <- list.map(variants)
      // We then apply the same conversion to all the variants accumulating
      // the invalid ones.
      case type_identifier(justin.pascal_case(variant)) {
        Ok(name) -> Ok(EnumVariant(name:, string_representation: variant))
        Error(_) -> Error(variant)
      }
    })

  case errors {
    // If any of the variants is invalid we fail reporting the error, otherwise
    // we can finally build the enum!
    [] ->
      case non_empty_list.from_list(variants) {
        Ok(variants) -> Ok(Enum(original_name: raw_name, name:, variants:))
        Error(_) -> Error(EnumWithNoVariants)
      }
    _ -> Error(InvalidEnumVariants(errors))
  }
}

// --- UTILS -------------------------------------------------------------------

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
