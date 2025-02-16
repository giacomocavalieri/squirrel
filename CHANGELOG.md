# CHANGELOG

## Unreleased

- Fixed a bug where code generation would include an unused import.
  ([Leah Ulmschneider](https://github.com/leah-u))

- Fixed a bug where the generated code would not be formatted properly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v3.0.0 - 2025-01-20

- Added a new command line option `check`, to check that the generated code is
  up to date with the sql queries.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The `exit` function from the `squirrel` module has been removed.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The CLI text displayed by Squirrel will never exceed the 80 chars line limit.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v2.1.0 - 2024-12-22

- The version requirement for `pog` has been changed to only permit v3.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- The generated code no longer relies on the `decode` package and now uses the
  `gleam/dynamic/decode` module from the `gleam_stdlib` package.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v2.0.5 - 2024-12-12

- Fixed a bug where queries with enum arrays would cause an error.
  ([Leah Ulmschneider](https://github.com/leah-u))

## v2.0.4 - 2024-12-04

- Replace deprecated `gleam/regex` module with `gleam/regexp`.
  ([Surya Rose](https://github.com/GearsDatapacks))

## v2.0.3 - 2024-11-28

- Improved error message when using a Postgres version that's too old.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v2.0.2 - 2024-11-20

- Fixed a bug where certain queries using conditions on foreign key would
  generate code with the wrong optional types.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- `drop` is now highlighted as a keyword in sql snippets in error messages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v2.0.1 - 2024-11-18

- Fixed a bug where certain queries would generate code with the wrong optional
  types.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v2.0.0 - 2024-11-11

- The generated code now uses the [`pog`](https://hexdocs.pm/pog/index.html)
  package instead of `gleam_pgo`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.8.1 - 2024-11-08

- Squirrel now errors if a query returns multiple columns with the same name
  instead of generating invalid Gleam code. For example:

  ```sql
  select
    1 as duplicate,
    2 as duplicate,
    3 as not_duplicate
  ```

  Results in the following error:

  ```txt
  Error: Duplicate names

      ╭─ query.sql
      │
    1 │ select
    2 │   1 as duplicate,
    3 │   2 as duplicate,
    4 │   3 as not_duplicate
      ┆

  This query returns multiple values sharing the same name: `duplicate`.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.8.0 - 2024-10-25

- Added support for `postgresql` URL scheme.
  ([Valentin Iancu](https://github.com/valentindiancu))

- Switched to `decode`'s new `zero` API for generated decoders.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added support for user-defined enums. An enum like this one:

  ```sql
  create type squirrel_colour as enum (
    'light_brown',
    'grey',
    'red'
  )
  ```

  Is automatically turned into the equivalent Gleam type:

  ```gleam
  pub type SquirrelColour {
    LightBrown
    Grey
    Red
  }
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.7.1 - 2024-09-22

- Fixed a bug where a query failing to parse would cause code generation for
  other queries to fail unexpectedly.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where a query starting with a semicolon would result in a crash
  instead of a proper syntax error.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where the generated code would have needless empty lines.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.7.0 - 2024-09-09

- Fixed a bug where an authentication error would result in a failure with a
  confusing error message.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added support for the `timestamp` type, represented as tuples
  `#(#(Int, Int, Int), #(Int Int Int))` with
  `#(#(year, month, day), #(hour, minute, second))`.
  ([Tommy Heffernan](https://github.com/tdheff))

- Fixed a bug where the SQL query displayed in an error would be dimmed out.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added more SQL keywords to syntax highlighting in error messages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.6.1 - 2024-09-04

- Fixed a bug where squirrel would not properly display some error messages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Improved syntax highlighting for sql queries displayed in error messages.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added a small hint in case no query is found, suggesting the correct project
  structure to follow.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.6.0 - 2024-09-03

- Added support for the `date` type, represented as a triple `#(Int, Int, Int)`
  with `#(year, month, day)`.
  ([Giovanni Paone](https://github.com/PavoJ))

## v1.5.0 - 2024-08-19

- Added support for the `bytea` type, represented as a `BitArray`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.4.0 - 2024-08-16

- Added support for the `uuid` type, represented as a `Uuid` (from the `youid`
  package).
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where `gleam run -m squirrel` would return a `0` exit code even in
  case of errors.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.3.2 - 2024-08-15

- Fixed a bug where the generated code would be missing a `gleam/list` import
  when dealing with Postgres arrays.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.3.1 - 2024-08-15

- Fixed a bug where Squirrel would panic if not able to establish a TCP
  connection to the postgres server. Now it gracefully handles the error by
  showing an appropriate error message.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.3.0 - 2024-08-13

- One can now use the `DATABASE_URL` env variable to specify a connection string
  that Squirrel will use to connect to the Postgres database.
  All the regular Postgres variables are still supported, but Squirrel will pick
  `DATABASE_URL` over those if it is set.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added support for the `jsonb` type, encoded as a `Json` (from the `gleam_json`
  package) and decoded as a Gleam `String`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added support for the `json` type, encoded as `Json` (from the `gleam_json`
  package) and decoded as a Gleam `String`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.2.0 - 2024-08-12

- Squirrel now supports the `SCRAM-SHA-256` authentication.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Squirrel now uses the name of your Gleam project as the default database name
  to connect to.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Fixed a bug where Squirrel couldn't generate code for queries that return no
  rows like `insert into`.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Avoid panicking when the authenticated user doesn't have the permission to
  access a given table.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.1.0 - 2024-08-11

- Squirrel now supports plaintext password authentication.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Squirrel now uses `"postgres"` as the default user in case the `PGUSER`
  environment variable is not set.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Added support for the `varchar` and `bpchar` types, mapped to Gleam `String`s.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

- Improved the error message in case Squirrel cannot connect to the Postgres
  server with the given database and username.
  Instead of a not-really-helpful `Cannot receive message: Closed` the error now
  properly explains what went wrong and hot to solve the issue:

  ```
  Error: Cannot connect

  I couldn't connect to database `database` with user `postgres`.

  Hint: You can change the default user and database by setting the `PGUSER` and
  `PGDATABASE` environment variables.
  ```

  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.0.1 - 2024-08-09

- Fixed a bug with the code generation of `Option` types.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.0.0 - 2024-08-09

- 🎉 First release!
