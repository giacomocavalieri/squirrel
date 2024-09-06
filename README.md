# 🐿️ squirrel - type safe SQL in Gleam

[![Package Version](https://img.shields.io/hexpm/v/squirrel)](https://hex.pm/packages/squirrel)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/squirrel/)

[![Squirrel showcase](https://github.com/user-attachments/assets/4abd308b-b191-4296-a0b6-093db0595794)](https://github.com/user-attachments/assets/4abd308b-b191-4296-a0b6-093db0595794)

## What's Squirrel?

If you need to talk with a database in Gleam you'll have to write something like
this:

```gleam
import gleam/pgo
import decode

pub type FindSquirrelRow {
  FindSquirrelRow(name: String, owned_acorns: Int)
}

/// Find a squirrel and its owned acorns given its name.
///
pub fn find_squirrel(db: pgo.Connection, name: String) {
  let squirrel_row_decoder =
    decode.into({
      use name <- decode.parameter
      use owned_acorns <- decode.parameter
      FindSquirrelRow(name: name, owned_acorns: owned_acorns)
    })
    |> decode.field(0, decode.string)
    |> decode.field(1, decode.int)

  "select name, owned_acorns
   from   squirrel
   where  name = $1"
  |> pgo.execute(db, [pgo.text(name)], squirrel_row_decoder)
}
```

This is probably fine if you have a few small queries but it can become quite
the burden when you have a lot of queries:

- The SQL query you write is just a plain string, you do not get syntax
  highlighting, auto formatting, suggestions... all the little niceties you
  would otherwise get if you where writing a plain `*.sql` file.
- This also means you loose the ability to run these queries on their own with
  other external tools, inspect them and so on.
- You have to manually keep in sync the decoder with the query's output.

One might be tempted to hide all of this by reaching for something like an ORM.
Squirrel proposes a different approach: instead of trying to hide the SQL it
_embraces it and leaves you in control._
You write the SQL queries in plain old `*.sql` files and Squirrel will take care
of generating all the corresponding functions.

A code snippet is worth a thousand words, so let's have a look at an example.
Instead of the hand written example shown earlier you can instead just write the
following query:

```sql
-- we're in file `src/squirrels/sql/find_squirrel.sql`
-- Find a squirrel and its owned acorns given its name.
select
  name,
  owned_acorns
from
  squirrel
where
  name = $1
```

And run `gleam run -m squirrel`. Just like magic you'll now have a type-safe
function `find_squirrel` you can use just as you'd expect:

```gleam
import gleam/pgo
import squirrels/sql

pub fn main() {
  let db = todo as "the pgo connection"
  // And it just works as you'd expect:
  let assert Ok(pgo.Returned(_rows_count, rows)) = sql.find_squirrel("sandy")
  let assert [FindSquirrelRow(name: "sandy", owned_acorns: 11_111)] = rows
}
```

Behind the scenes Squirrel generates the decoders and functions you need; and
it's pretty-printed, standard Gleam code (actually it's exactly like the hand
written example I showed you earlier)!
So now you get the best of both worlds:

- You don't have to take care of keeping encoders and decoders in sync, Squirrel
  does that for you.
- And you're not compromising on type safety either: Squirrel is able to
  understand the types of your query and produce a correct decoder.
- You can stick to writing plain SQL in `*.sql` files. You'll have better
  editor support, syntax highlighting and completions.
- You can run each query on its own: need to `explain` a query?
  No big deal, it's just a plain old `*.sql` file.

## Usage

First you'll need to add Squirrel to your project as a dev dependency:

```sh
gleam add squirrel --dev

# Remember to add these packages if you haven't yet, they are needed by the
# generated code to run and decode the read rows!
gleam add gleam_pgo
gleam add decode
```

Then you can ask it to generate code running the `squirrel` module:

```sh
gleam run -m squirrel
```

And that's it! As long as you follow a couple of conventions Squirrel will just
work:

- Squirrel will look for all `*.sql` files in any `sql` directory under your
  project's `src` directory.
- Each `sql` directory will be turned into a single Gleam module containing a
  function for each `*.sql` file inside it. The generated Gleam module is going
  to be located in the same directory as the corresponding `sql` directory and
  it's name is `sql.gleam`.
- Each `*.sql` file _must contain a single SQL query._ And the name of the file
  is going to be the name of the corresponding Gleam function to run that query.

> Let's make an example. Imagine you have a Gleam project that looks like this
>
> ```txt
> ├── src
> │   ├── squirrels
> │   │   └── sql
> │   │       ├── find_squirrel.sql
> │   │       └── list_squirrels.sql
> │   └── squirrels.gleam
> └── test
>     └── squirrels_test.gleam
> ```
>
> Running `gleam run -m squirrel` will create a `src/squirrels/sql.gleam` file
> defining two functions `find_squirrel` and `list_squirrels` you can then
> import and use in your code.

### Talking to the database

In order to understand the type of your queries, Squirrel needs to connect to
the Postgres server where the database is defined. To connect, it will read the
`DATABASE_URL` env variable that has to be a valid connection string with the
following format:

```txt
postgres://user:password@host:port/database
```

If a `DATABASE_URL` variable is not set, Squirrel will instead read your
[Postgres env variables](https://www.postgresql.org/docs/current/libpq-envars.html)
and use the following defaults if one is not set:

- `PGHOST`: `"localhost"`
- `PGPORT`: `5432`
- `PGUSER`: `"root"`
- `PGDATABASE`: the name of your Gleam project
- `PGPASSWORD`: `""`

## Supported types

Squirrel takes care of the mapping between Postgres types and Gleam types.
This is needed in two places:

- Gleam values need to be _encoded_ into Postgres values when you're filling in
  the holes of a prepared statement (`$1`, `$2`, ...)
- Postgres values need to be _decoded_ into a Gleam ones when you're reading the
  rows returned by a query.

The types that are currently supported are:

| postgres type                                          | encoded as                                                                                      | decoded as                                                                                         |
| ------------------------------------------------------ | ----------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `bool`                                                 | `Bool`                                                                                          | `Bool`                                                                                             |
| `text`, `char`, `bchar`, `varchar`                     | `String`                                                                                        | `String`                                                                                           |
| `float4`, `float8`, `numeric`                          | `Float`                                                                                         | `Float`                                                                                            |
| `int2`, `int4`, `int8`                                 | `Int`                                                                                           | `Int`                                                                                              |
| `json`, `jsonb`                                        | [`Json`](https://hexdocs.pm/gleam_json/gleam/json.html#Json)                                    | `String`                                                                                           |
| `uuid`                                                 | [`Uuid`](https://hexdocs.pm/youid/youid/uuid.html#Uuid)                                         | [`Uuid`](https://hexdocs.pm/youid/youid/uuid.html#Uuid)                                            |
| `bytea`                                                | `BitArray`                                                                                      | `BitArray`                                                                                         |
| `date`                                                 | `#(Int, Int, Int)` with `#(year, month, day)`                                                   | `#(Int, Int, Int)` with `#(year, month, day)`                                                      |
| `timestamp`                                            | `#(#(Int, Int, Int), (#(Int, Int, Int))` with `#(#(year, month, day), #(hour, minute, second))` | `#(#(Int, Int, Int), (#(Int, Int, Int))` with `#(#(year, month, day), #(hour, minute, second))`    |
| `<type>[]` (where `<type>` is any supported type)      | `List(<type>)`                                                                                  | `List(<type>)`                                                                                     |

## FAQ

### What flavour of SQL does squirrel support?

Squirrel only has support for Postgres.

### Why isn't squirrel configurable in any way?

By going the "convention over configuration" route, Squirrel enforces that all
projects adopting it will always have the same structure.
If you need to contribute to a project using Squirrel you'll immediately know
which directories and modules to look for.

This makes it easier to get started with a new project and cuts down on all the
bike shedding: _"Where should I put my queries?",_
_"How many queries should go in on file?",_ ...

## References

This package draws a lot of inspiration from the amazing
[yesql](https://github.com/krisajenkins/yesql) and
[sqlx](https://github.com/launchbadge/sqlx).

## Contributing

If you think there’s any way to improve this package, or if you spot a bug don’t
be afraid to open PRs, issues or requests of any kind! Any contribution is
welcome 💜
