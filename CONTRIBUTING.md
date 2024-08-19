# Contributing

If you're reading this, thank you so much for trying to contribute to
`squirrel`!
I tried to do my best and comment the code as much as possible and make it easy
to understand. Each module also starts with a small comment to explain what it
does, so it should be easier to dive in the codebase.

> 💡 If you feel like some pieces of code are not commented enough or are too
> obscure, than that's a bug! Please do reach out, I'd love to hear your
> feedback and make `squirrel` easier to contribute to!

## Running the tests

Most of the tests are snapshot tests that directly call the `postgres.main`
function to let it type the queries. In order to do that `squirrel` will have to
connect to a postgres server that must be running during the tests.

- In CI this is taken care of automatically

- Locally you can run the tests using Docker Compose to start a postgres db or using a manual setup.
- To use Docker compose:
  - In one terminal run `docker compose up`
  - In another terminal run the tests
- Without Docker Compose, you will need a PG instance running with:
  - There must be a user called `squirrel_test`
  - It must be able to read and write to a database called `squirrel_test`
  - It will use the empty password to connect at `localhost`'s port `5432`

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
