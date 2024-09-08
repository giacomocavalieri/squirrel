import birdie
import filepath
import glam/doc
import gleam/dynamic
import gleam/list
import gleam/pgo
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
);
"
    |> pgo.execute(db, [], dynamic.dynamic)

  let assert Ok(_) =
    "
create table if not exists jsons(
  id bigserial primary key,
  json json,
  jsonb jsonb
)
"
    |> pgo.execute(db, [], dynamic.dynamic)

  pgo.disconnect(db)
}

// --- ASSERTION HELPERS -------------------------------------------------------

fn should_error(query: String) -> String {
  let assert Ok(#([], errors)) = type_queries([#("query", query)])

  list.map(errors, error.to_doc)
  |> doc.join(with: doc.lines(2))
  |> doc.to_string(80)
}

fn should_codegen(query: String) -> String {
  should_codegen_queries([#("query", query)])
}

fn should_codegen_queries(queries: List(#(String, String))) -> String {
  // We assert everything went smoothly and we have no errors in the query.
  let assert Ok(#(queries, [])) = type_queries(queries)
  query.generate_code(queries, "v-test")
}

fn type_queries(
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

pub fn json_decoding_test() {
  "select '{\"a\": 1}'::json as res"
  |> should_codegen
  |> birdie.snap(title: "json decoding")
}

pub fn json_encoding_test() {
  "insert into jsons(json) values ($1)"
  |> should_codegen
  |> birdie.snap(title: "json encoding")
}

pub fn jsonb_decoding_test() {
  "select '{\"a\": 1}'::jsonb as res"
  |> should_codegen
  |> birdie.snap(title: "jsonb decoding")
}

pub fn jsonb_encoding_test() {
  "insert into jsons(jsonb) values($1)"
  |> should_codegen
  |> birdie.snap(title: "jsonb encoding")
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

pub fn left_join_columns_are_optional_test() {
  "
select
  s1.name as not_optional,
  s2.name as optional
from
  squirrel s1
  left join squirrel s2 using(name)
"
  |> should_codegen
  |> birdie.snap(title: "left join columns are optional")
}

pub fn right_join_columns_are_optional_test() {
  "
select
  s1.name as optional,
  s2.name as not_optional
from
  squirrel s1
  right join squirrel s2 using(name)
"
  |> should_codegen
  |> birdie.snap(title: "right join columns are optional")
}

pub fn full_join_columns_are_optional_test() {
  "
select
  s1.name as optional1,
  s2.name as optional2
from
  squirrel s1
  full join squirrel s2 using(name)
"
  |> should_codegen
  |> birdie.snap(title: "full join columns are optional")
}

pub fn uuid_decoding_test() {
  "select gen_random_uuid() as res"
  |> should_codegen
  |> birdie.snap(title: "uuid decoding")
}

pub fn uuid_encoding_test() {
  "select true as res where $1 = gen_random_uuid()"
  |> should_codegen
  |> birdie.snap(title: "uuid encoding")
}

pub fn bytea_decoding_test() {
  "select 'aaa'::bytea as res"
  |> should_codegen
  |> birdie.snap(title: "bytea decoding")
}

pub fn bytea_encoding_test() {
  "select true as res where $1 = 'aaa'::bytea"
  |> should_codegen
  |> birdie.snap(title: "bytea encoding")
}

pub fn date_decoding_test() {
  "select 'Jan-2-1970'::date as res"
  |> should_codegen
  |> birdie.snap(title: "date decoding")
}

pub fn date_encoding_test() {
  "select true as res where $1 = 'Jan-2-1970'::date"
  |> should_codegen
  |> birdie.snap(title: "date encoding")
}

pub fn timestamp_decoding_test() {
  "select 'Jan-2-1970 12:34:56'::timestamp as res"
  |> should_codegen
  |> birdie.snap(title: "timestamp decoding")
}

pub fn timestamp_encoding_test() {
  "select true as res where $1 = 'Jan-2-1970 12:34:56'::timestamp"
  |> should_codegen
  |> birdie.snap(title: "timestamp encoding")
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

pub fn fields_appear_in_the_order_they_have_in_the_select_list_test() {
  "
select true as first, 1 as second, 'wibble' as third
"
  |> should_codegen
  |> birdie.snap(
    title: "fields appear in the order they have in the select list",
  )
}

pub fn using_uuids_more_than_once_results_in_a_single_uuid_decoder_helper_test() {
  [#("one", "select gen_random_uuid()"), #("other", "select gen_random_uuid()")]
  |> should_codegen_queries
  |> birdie.snap(
    title: "using uuids more than once results in a single uuid decoder helper",
  )
}

pub fn query_that_needs_more_than_a_single_helper_function_test() {
  "select gen_random_uuid(), 'Jan-2-1970'::date"
  |> should_codegen
  |> birdie.snap(title: "query that needs more than a single helper function")
}

// --- ERRROR TESTS ------------------------------------------------------------
// This is a group of tests to ensure that the errors look good when something
// goes wrong.
//
// > ⚠️ For now I'm just testing errors related to the query parsing and name
// > checking as I still have to find a good way to end-to-end test connection
// > stuff...
//

pub fn column_with_invalid_name_test() {
  "
select
  name as \"not a gleam name\"
from
  squirrel
"
  |> should_error
  |> birdie.snap(title: "column with invalid name")
}

pub fn query_with_syntax_error_test() {
  "
select
  name wibble wobble
from
  squirrel
"
  |> should_error
  |> birdie.snap(title: "query with syntax error")
}

pub fn query_with_table_that_doesnt_exist_test() {
  "
select
  name
from
  i_do_not_exist
"
  |> should_error
  |> birdie.snap(title: "query with table that doesn't exist")
}

pub fn query_with_column_that_doesnt_exist_test() {
  "
select
  i_do_not_exist
from
  squirrel
"
  |> should_error
  |> birdie.snap(title: "query with column that doesn't exist")
}

// --- REGRESSIONS -------------------------------------------------------------
// Bugs reported from GitHub issues so I make sure those will no longer pop up.
//

// https://github.com/giacomocavalieri/squirrel/issues/8
pub fn when_using_a_list_as_argument_the_list_module_is_imported_test() {
  "
select
  true as res
where
  $1 = array[1, 2, 3]
"
  |> should_codegen
  |> string.contains("import gleam/list")
}

// https://github.com/giacomocavalieri/squirrel/issues/19
pub fn non_existing_constraint_error_message_test() {
  "
insert into squirrel values ($1, $2)
on conflict on constraint wobble do nothing;
"
  |> should_error
  |> birdie.snap(
    title: "when a constraint doesn't exist there should be an error message",
  )
}
