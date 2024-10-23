# Contributing

If you're reading this, thank you so much for trying to contribute to
`squirrel`!
I tried to do my best and comment the code as much as possible and make it easy
to understand. Each module also starts with a small comment to explain what it
does, so it should be easier to dive in the codebase.

> 💡 If you feel like some pieces of code are not commented enough or are too
> obscure, than that's a bug! Please do reach out, I'd love to hear your
> feedback and make `squirrel` easier to contribute to!

## Adding support for a new type

If you want to add support for a new postgres type there's a couple of steps to
go through:

- Add a new case to `squirrel/internal/database/postgres.pg_to_gleam_type`
  turning the postgres type to the corresponding Gleam type
- If the Gleam type you're looking for is not defined then you'll also need to
  add a new variant to the `squirrel/internal/gleam.Type` type
- And then the compiler errors will guide you through all the needed steps to
  support the new Gleam type. In short:
  - You'll need to define how that is decoded adding a case to the
    `squirrel/internal/query.gleam_type_to_decoder` function.
    This is used when the type has to be read from the database
  - You'll need to define how that is encoded adding a case to the
    `squirrel/internal/query.gleam_type_to_encoder` function.
    This is used when the type has to be passed in as one of the query holes
  - You'll need to define how that is written down in a type signature adding a
    case to the `squirrel/internal/query.gleam_type_to_field_type` function.
    This is used when the type is in the values returned by the query to
    write down the type of the corresponding field
- And don't forget to add some tests and integration tests :)

## Writing tests

`squirrel` uses a lot of snapshot tests, to add new tests for the code
generation bits you can have a look and copy the existing ones.
There's no hard requirements but I have some suggestion to write good snapshot
tests:

- Have at most one snapshot per test function
- Try to keep the snapshots as small as possible.
  Ideally one snapshot should assert a single property of the generated code so
  that it is easier to focus on a specific aspect of the code when reviewing it
- Use a long descriptive title for the snapshots: a title should describe what
  one expects to see in the produced snapshot to guide the review process

## Integration tests

Squirrel also has some integration tests to check that the generated code
actually compiles and produces the expected results.

- To add a new integration test you can add a new item to the
  `integration_tests` list in `test/integration_test.gleam`
- To run the tests you can run the `./integration_test` script in this project.
  Read below to set up all the required bits for tests to work

## Running the tests

Most of the tests are snapshot tests that directly call the `postgres.main`
function to let it type the queries. In order to do that `squirrel` will have to
connect to a Postgres server at `localhost`'s port `5432`.

- In CI this is taken care of automatically
- Locally you have two options:
  - Use Docker Compose: the project comes with a `docker-compose.yaml` file that
    sets up the instance, so you can run `docker compose up` to start it and run
    your tests
  - Manually set up a Postgres server: you'll have to make sure you have a
    server running with a user called `squirrel_test` that must be able to read
    and write to a database called `squirrel_test`
