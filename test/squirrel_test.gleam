import birdie
import filepath
import gleam/dynamic
import gleam/list
import gleam/pgo
import gleam/set
import gleam/string
import gleeunit
import simplifile
import squirrel/internal/database/postgres
import squirrel/internal/error.{type Error}
import squirrel/internal/query.{type TypedQuery}
import temporary

pub fn main() {
  setup_database()
  gleeunit.main()
}

// --- TEST SETUP --------------------------------------------------------------

const host = "localhost"

const user = "squirrel_test"

const database = "squirrel_test"

const port = 5432

fn setup_database() {
  let config =
    pgo.Config(
      ..pgo.default_config(),
      port: port,
      user: user,
      host: host,
      database: database,
    )
  let db = pgo.connect(config)

  let assert Ok(_) =
    "
create table if not exists squirrel(
  name text primary key,
  acorns int
)
"
    |> pgo.execute(db, [], dynamic.dynamic)

  pgo.disconnect(db)
}

// --- ASSERTION HELPERS -------------------------------------------------------

fn should_codegen(query: String) -> String {
  // We assert everything went smoothly and we have no errors in the query.
  let assert Ok(#(queries, [])) = codegen_queries([#("query", query)])

  let #(queries, imports) = {
    use #(queries, imports), query <- list.fold(queries, #([], set.new()))
    let #(query, query_imports) = query.generate_code("v-test", query)
    #([query, ..queries], imports |> set.union(query_imports))
  }

  set.to_list(imports)
  |> list.sort(string.compare)
  |> string.join(with: "\n")
  |> string.append("\n\n")
  |> string.append(
    list.reverse(queries)
    |> string.join(with: "\n\n"),
  )
}

fn codegen_queries(
  queries: List(#(String, String)),
) -> Result(#(List(TypedQuery), List(Error)), Error) {
  // If there's any error with the temporary package we just fail the test,
  // there's no reason to try and keep going.
  let assert Ok(result) = {
    use temp_dir <- temporary.create(temporary.directory())

    // We parse all the queries.
    let queries = {
      use #(file, query) <- list.map(queries)
      let out_file = filepath.join(temp_dir, file <> ".sql")
      let assert Ok(_) = simplifile.write(to: out_file, contents: query)
      let assert Ok(query) = query.from_file(out_file)
      // We manually change the file name here: we do not want to use the full
      // out_file name in tests because that will change across different runs,
      // causing the snapshot tests to fail.
      let query = query.UntypedQuery(..query, file: file <> ".sql")
      query
    }

    // We can then ask squirrel to type check all the queries.
    postgres.main(
      queries,
      postgres.ConnectionOptions(
        host: host,
        port: port,
        user: user,
        database: database,
        password: "",
        timeout: 1000,
      ),
    )
  }

  result
}

// --- ENCODING/DECODING CODEGEN TESTS -----------------------------------------
// This is a group of tests to ensure the generated encoders/decoders are what
// we expect for all the supported data types.
//

pub fn int_decoding_test() {
  "select 11 as res"
  |> should_codegen
  |> birdie.snap(title: "int decoding")
}

pub fn int_encoding_test() {
  "select true as res where $1 = 11"
  |> should_codegen
  |> birdie.snap(title: "int encoding")
}

pub fn float_decoding_test() {
  "select 1.1 as res"
  |> should_codegen
  |> birdie.snap(title: "float decoding")
}

pub fn float_encoding_test() {
  "select true as res where $1 = 1.1"
  |> should_codegen
  |> birdie.snap(title: "float encoding")
}

pub fn string_decoding_test() {
  "select 'wibble' as res"
  |> should_codegen
  |> birdie.snap(title: "string decoding")
}

pub fn string_encoding_test() {
  "select true as res where $1 = 'wibble'"
  |> should_codegen
  |> birdie.snap(title: "string encoding")
}

pub fn char_decoding_test() {
  "select 'a'::char as res"
  |> should_codegen
  |> birdie.snap(title: "char decoding")
}

pub fn char_encoding_test() {
  "select true as res where $1 = 'a'::char"
  |> should_codegen
  |> birdie.snap(title: "char encoding")
}

pub fn varchar_decoding_test() {
  "select 'wibble'::varchar(6) as res"
  |> should_codegen
  |> birdie.snap(title: "varchar decoding")
}

pub fn varchar_encoding_test() {
  "select true as res where $1 = 'wibble'::varchar(6)"
  |> should_codegen
  |> birdie.snap(title: "varchar encoding")
}

pub fn bool_decoding_test() {
  "select true as res"
  |> should_codegen
  |> birdie.snap(title: "bool decoding")
}

pub fn bool_encoding_test() {
  "select true as res where $1 = true"
  |> should_codegen
  |> birdie.snap(title: "bool encoding")
}

pub fn array_decoding_test() {
  "select array[1, 2, 3] as res"
  |> should_codegen
  |> birdie.snap(title: "array decoding")
}

pub fn array_encoding_test() {
  "select true as res where $1 = array[1, 2, 3]"
  |> should_codegen
  |> birdie.snap(title: "array encoding")
}

pub fn optional_decoding_test() {
  "select acorns from squirrel"
  |> should_codegen
  |> birdie.snap(title: "optional decoding")
}

// --- CODEGEN STRUCTURE TESTS -------------------------------------------------
// This is a group of tests to ensure the generated code has some specific
// structure (e.g. the names and comments are what we expect...)
//

pub fn query_with_comment_test() {
  "
-- This is a comment
select true as res
"
  |> should_codegen
  |> birdie.snap(title: "query with comment")
}

pub fn query_with_multiline_comment_test() {
  "
-- This is a comment
-- that goes over multiple lines!
select true as res
"
  |> should_codegen
  |> birdie.snap(title: "query with multiline comment")
}

pub fn generated_type_has_the_same_name_as_the_function_but_in_pascal_case_test() {
  "
select true as res
"
  |> should_codegen
  |> birdie.snap(
    title: "generated type has the same name as the function but in pascal case",
  )
}

pub fn generated_type_fields_are_labelled_with_their_name_in_the_select_list_test() {
  "
select
  acorns,
  name as squirrel_name
from
  squirrel
"
  |> should_codegen
  |> birdie.snap(
    title: "generated type fields are labelled with their name in the select list",
  )
}

pub fn insert_with_no_returned_values_returns_just_nil_and_doesnt_define_a_type_test() {
  "insert into squirrel values ('sandy', 1000)"
  |> should_codegen
  |> birdie.snap(
    title: "insert with no returned values returns just nil and doesnt define a type",
  )
}
