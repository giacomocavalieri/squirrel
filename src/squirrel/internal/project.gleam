//// A set of utilities to work within a Gleam project.
////

import filepath
import gleam/result
import simplifile
import tom

/// Returns the project's name, read from the `gleam.toml` file.
///
pub fn name() -> Result(String, Nil) {
  let configuration_path = filepath.join(root(), "gleam.toml")

  use configuration <- try_nil(simplifile.read(configuration_path))
  use toml <- try_nil(tom.parse(configuration))
  use name <- try_nil(tom.get_string(toml, ["name"]))
  Ok(name)
}

fn try_nil(
  result: Result(a, b),
  then do: fn(a) -> Result(c, Nil),
) -> Result(c, Nil) {
  result.try(result.replace_error(result, Nil), do)
}

/// Finds the path of the project's `src` directory.
/// This recursively walks up from the current directory until it finds a
/// `gleam.toml` and builds it from there.
///
pub fn src() -> String {
  filepath.join(root(), "src")
}

/// Finds the path of the project's `test` directory.
/// This recursively walks up from the current directory until it finds a
/// `gleam.toml` and builds it from there.
///
pub fn test_() -> String {
  filepath.join(root(), "test")
}

/// Finds the path of the project's `dev` directory.
/// This recursively walks up from the current directory until it finds a
/// `gleam.toml` and builds it from there.
///
pub fn dev() -> String {
  filepath.join(root(), "dev")
}

/// Finds the path leading to the project's root folder. This recursively walks
/// up from the current directory until it finds a `gleam.toml`.
///
fn root() -> String {
  find_root(".")
}

fn find_root(path: String) -> String {
  let toml = filepath.join(path, "gleam.toml")

  case simplifile.is_file(toml) {
    Ok(False) | Error(_) -> find_root(filepath.join("..", path))
    Ok(True) -> path
  }
}
