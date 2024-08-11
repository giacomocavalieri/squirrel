# CHANGELOG

## Unreleased

- Squirrel now supports the `SCRAM-SHA-256` authentication.
  ([Giacomo Cavalieri](https://github.com/giacomocavalieri))

## v1.1.0 - 2024-11-09

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
