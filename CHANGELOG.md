# CHANGELOG

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
