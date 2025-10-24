import birdie
import filepath
import glam/doc
import gleam/erlang/process
import gleam/list
import gleam/string
import gleeunit
import pog
import simplifile
import squirrel
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

pub fn test_config() -> pog.Config {
  let name = process.new_name("test")

  pog.default_config(name)
  |> pog.port(port)
  |> pog.user(user)
  |> pog.host(host)
  |> pog.database(database)
}

fn setup_database() {
  let assert Ok(actor) = pog.start(test_config())
  let db = actor.data

  let assert Ok(_) =
    "
create table if not exists squirrel(
  name text primary key,
  acorns int
)"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
create table if not exists jsons(
  id bigserial primary key,
  json json,
  jsonb jsonb
)"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
do $$ begin
  if not exists (select * from pg_type where typname = 'squirrel_colour') then
    create type squirrel_colour as enum ('red', 'grey', 'light brown');
  end if;
end $$;"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
do $$ begin
  if not exists (select * from pg_type where typname = '1 invalid enum') then
    create type \"1 invalid enum\" as enum ('value');
  end if;
end $$;"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
do $$ begin
  if not exists (select * from pg_type where typname = 'invalid_variant') then
    create type invalid_variant as enum ('1 invalid value');
  end if;
end $$;"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
do $$ begin
  if not exists (select * from pg_type where typname = 'no_variants') then
    create type no_variants as enum ();
  end if;
end $$;"
    |> pog.query
    |> pog.execute(db)

  // https://github.com/giacomocavalieri/squirrel/issues/41
  let assert Ok(_) =
    "
create table if not exists users_issue41(
  user_id bigserial primary key
)
    "
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
create table if not exists profile_issue41(
  profile_id bigserial primary key,
  user_id bigserial not null,
  roles text not null
);"
    |> pog.query
    |> pog.execute(db)

  // https://github.com/giacomocavalieri/squirrel/issues/43
  let assert Ok(_) =
    "
create table if not exists category_issue43(
  category_id bigserial primary key,
  name text not null
);"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
create table if not exists site_issue43(
  site_id bigserial primary key
)"
    |> pog.query
    |> pog.execute(db)
  let assert Ok(_) =
    "
create table if not exists item_issue43 (
  item_id bigserial primary key,
  category_id bigserial references category_issue43(category_id),
  site_id bigserial references site_issue43(site_id)
)"
    |> pog.query
    |> pog.execute(db)

  // https://github.com/giacomocavalieri/squirrel/issues/75
  let assert Ok(_) =
    "
create table if not exists categories_issue75 (
  id uuid primary key,
  name varchar(70) not null,
  parent_id uuid not null
);
  "
    |> pog.query
    |> pog.execute(db)

  // https://github.com/giacomocavalieri/squirrel/issues/114
  let assert Ok(_) =
    "
do $$ begin
  if not exists (select * from pg_type where typname = 'issue_114_aaaaaaaaaaaaaaaaa') then
    create type issue_114_aaaaaaaaaaaaaaaaa as enum('wibble');
  end if;
end $$;"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
create table if not exists items_issue75 (
  id uuid primary key,
  name varchar(70) not null
);
"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "
create table if not exists items_categories_issue75 (
  item_id uuid not null,
  category_id uuid not null,
  primary key (item_id, category_id)
);"
    |> pog.query
    |> pog.execute(db)

  let assert Ok(_) =
    "create extension if not exists citext;"
    |> pog.query
    |> pog.execute(db)

  Nil
}

// --- ASSERTION HELPERS -------------------------------------------------------

fn should_error(query: String) -> String {
  should_error_queries([#("query", query)])
}

fn should_error_queries(queries: List(#(String, String))) -> String {
  let assert Ok(#([], errors)) = type_queries(queries)

  list.map(errors, error.to_doc)
  |> doc.join(with: doc.lines(2))
  |> doc.to_string(80)
}

fn should_codegen(query: String) -> String {
  should_codegen_queries([#("query", query)])
}

fn should_codegen_with_comments(query: String) {
  should_codegen_queries_options(
    [#("query", query)],
    CodegenOptions(strip_comments: False),
  )
}

fn should_codegen_queries(queries: List(#(String, String))) -> String {
  should_codegen_queries_options(queries, CodegenOptions(strip_comments: True))
}

fn should_codegen_queries_with_comments(
  queries: List(#(String, String)),
) -> String {
  should_codegen_queries_options(queries, CodegenOptions(strip_comments: False))
}

fn should_codegen_queries_options(
  queries: List(#(String, String)),
  options: CodegenOptions,
) {
  // We assert everything went smoothly and we have no errors in the query.
  let assert Ok(#(queries, [])) = type_queries(queries)
  let code =
    query.generate_code("v-test", for: queries, from: "./test-directory")

  let CodegenOptions(strip_comments:) = options
  case strip_comments {
    False -> code
    True ->
      string.split(code, on: "\n")
      |> list.filter(keeping: fn(line) { !string.starts_with(line, "//") })
      |> string.join(with: "\n")
  }
}

type CodegenOptions {
  CodegenOptions(
    /// Removes all lines starting with `//` from the generated code.
    strip_comments: Bool,
  )
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
        timeout_seconds: 1,
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

pub fn numeric_decoding_test() {
  "select 1.1::numeric as res"
  |> should_codegen
  |> birdie.snap(title: "numeric decoding")
}

pub fn numeric_encoding_test() {
  "select true as res where $1 = 1.1::numeric"
  |> should_codegen
  |> birdie.snap(title: "numeric encoding")
}

pub fn float4_decoding_test() {
  "select 1.1::float4 as res"
  |> should_codegen
  |> birdie.snap(title: "float4 decoding")
}

pub fn float4_encoding_test() {
  "select true as res where $1 = 1.1::float4"
  |> should_codegen
  |> birdie.snap(title: "float4 encoding")
}

pub fn float8_decoding_test() {
  "select 1.1::float8 as res"
  |> should_codegen
  |> birdie.snap(title: "float8 decoding")
}

pub fn float8_encoding_test() {
  "select true as res where $1 = 1.1::float8"
  |> should_codegen
  |> birdie.snap(title: "float8 encoding")
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

pub fn citext_decoding_test() {
  "select 'wibble'::citext as res"
  |> should_codegen
  |> birdie.snap(title: "citext decoding")
}

pub fn citext_encoding_test() {
  "select true as res where $1 = 'wibble'::citext"
  |> should_codegen
  |> birdie.snap(title: "citext encoding")
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

pub fn time_of_day_decoding_test() {
  "select '11:10:01'::time as res"
  |> should_codegen
  |> birdie.snap(title: "time decoding")
}

pub fn time_of_day_encoding_test() {
  "select true as res where $1 = '11:10:00'::time"
  |> should_codegen
  |> birdie.snap(title: "time encoding")
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

pub fn enum_decoding_test() {
  "select 'red'::squirrel_colour"
  |> should_codegen
  |> birdie.snap(title: "enum decoding")
}

pub fn enum_encoding_test() {
  "select 1 as res where $1 = 'red'::squirrel_colour"
  |> should_codegen
  |> birdie.snap(title: "enum encoding")
}

pub fn enum_array_decoding_test() {
  "select array['red'::squirrel_colour] as res"
  |> should_codegen
  |> birdie.snap(title: "enum array decoding")
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
  |> should_codegen_with_comments
  |> birdie.snap(title: "query with comment")
}

pub fn query_with_multiline_comment_test() {
  "
-- This is a comment
-- that goes over multiple lines!
select true as res
"
  |> should_codegen_with_comments
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
  "select true as first, 1 as second, 'wibble' as third"
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

pub fn query_that_needs_utils_and_enums_has_two_sections_test() {
  "select 'red'::squirrel_colour, 'Jan-2-1970'::date"
  |> should_codegen
  |> birdie.snap(title: "query that needs utils and enums has two sections")
}

pub fn query_with_quoted_string_is_properly_escaped_test() {
  "select 1 as \"result\""
  |> should_codegen
  |> birdie.snap(title: "query with quoted string is properly escaped")
}

// https://github.com/giacomocavalieri/squirrel/issues/77
pub fn decode_success_with_long_builder_is_properly_formatted_not_breaking_it_on_a_different_line_test() {
  "select
  1 as first_column,
  2 as second_column,
  3 as third_column,
  4 as fourth_column,
  5 as fifth_column,
  6 as sixth_column;"
  |> should_codegen
  |> birdie.snap(
    title: "decode.success with long builder is properly formatted not breaking it on a different line",
  )
}

// https://github.com/giacomocavalieri/squirrel/pull/87#issuecomment-2994056542
pub fn decode_success_with_long_builder_close_to_80_chars_is_properly_formatted_not_breaking_it_on_a_different_line_test() {
  "select
  1 as aaaaaaaa,
  2 as bbbbbbbbbbbbbbb,
  3 as ccc,
  4 as dddddddddddddddd;"
  |> should_codegen
  |> birdie.snap(
    title: "decode.success with long builder close to 80 chars is properly formatted not breaking it on a different line",
  )
}

pub fn queries_are_sorted_alphabetically_test() {
  should_codegen_queries([
    #("last", "select 1 as wibble"),
    #("first", "select 1 as wibble"),
  ])
  |> birdie.snap(title: "queries are sorted alphabetically")
}

pub fn query_with_many_arguments_returning_nil_comment_test() {
  "
with a as (
  select $1, *
  from unnest(
    $2::text[],
    $3::text[],
    $4::text[],
    $5::text[],
    $6::text[],
    $7::text[],
    $8::text[],
    $9::text[]
  )
)
select
"
  |> should_codegen_with_comments
  |> birdie.snap(title: "query with many arguments returning nil")
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

pub fn timestampz_has_a_nice_hint_nudging_to_use_a_timestamp_test() {
  "select timestamp with time zone '11 oct 1998'"
  |> should_error
  |> birdie.snap(title: "timestampz has a nice hint nudging to use a timestamp")
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

pub fn enum_with_invalid_name_test() {
  "select 'value'::\"1 invalid enum\" as res"
  |> should_error
  |> birdie.snap(title: "enum with invalid name")
}

pub fn enum_with_invalid_variant_test() {
  "select '1 invalid value'::invalid_variant as res"
  |> should_error
  |> birdie.snap(title: "enum with invalid variant")
}

pub fn enum_with_no_variants_is_rejected_test() {
  "select $1::no_variants as res"
  |> should_error
  |> birdie.snap(title: "enum with no variants is rejected")
}

pub fn query_returning_columns_with_same_name_test() {
  "select
    1 as duplicate,
    2 as duplicate,
    3 as not_duplicate"
  |> should_error
  |> birdie.snap(title: "query returning columns with same name")
}

pub fn query_returning_columns_with_same_names_test() {
  "select
    1 as duplicate_1,
    2 as duplicate_2,
    3 as not_duplicate,
    4 as duplicate_1,
    5 as duplicate_2"
  |> should_error
  |> birdie.snap(title: "query returning columns with same names")
}

// --- CHECKING ----------------------------------------------------------------
// Tests for the `check` command.
//

pub fn checking_two_identical_snippets_of_code_test() {
  let code = should_codegen_with_comments("select 1 as number")
  let assert squirrel.Same = squirrel.compare_code_snippets(code, code)
}

pub fn if_code_snippets_differ_by_formatting_they_are_the_same_test() {
  let expected_code = should_codegen_with_comments("select 1 as number")
  let actual_code = expected_code |> string.replace(each: "\n", with: "\n ")
  let assert squirrel.Same =
    squirrel.compare_code_snippets(expected_code, actual_code)
}

pub fn if_code_snippets_differ_by_comments_they_are_the_same_test() {
  let expected_code = should_codegen_with_comments("select 1 as number")
  let actual_code = "// Comment!\n" <> expected_code
  let assert squirrel.Same =
    squirrel.compare_code_snippets(expected_code, actual_code)
}

pub fn comparing_different_snippets_of_code_test() {
  let expected_code = should_codegen_with_comments("select 1 as number")
  let actual_code = should_codegen_with_comments("select 2 as number")
  let assert squirrel.Different =
    squirrel.compare_code_snippets(expected_code, actual_code)
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

// https://github.com/giacomocavalieri/squirrel/issues/24
pub fn query_starting_with_a_semicolon_does_not_crash_test() {
  should_codegen(";select 1 as result")
  |> birdie.snap(title: "query starting with a semicolon does not crash")
}

// https://github.com/giacomocavalieri/squirrel/issues/29
pub fn there_is_only_one_empty_line_between_code_generated_for_different_queries_test() {
  should_codegen_queries_with_comments([
    #("one", "select 1 as res"),
    #("two", "select 2 as res"),
  ])
  |> birdie.snap(
    title: "there is only one empty line between code generated for different queries",
  )
}

// https://github.com/giacomocavalieri/squirrel/issues/28
pub fn a_query_failing_does_not_change_the_other_query_error_test() {
  should_error_queries([
    #("wibble", "select wibble from wibble"),
    #("wobble", "select wobble from wobble"),
  ])
  |> birdie.snap(
    title: "a query failing does not change the other query's error",
  )
}

pub fn a_query_failing_does_not_change_the_other_query_error_2_test() {
  should_error_queries([
    #("wibble", "error! select 1 as res"),
    #("wobble", "select wobble from wobble"),
  ])
  |> birdie.snap(
    title: "a query failing does not change the other query's error 2",
  )
}

// https://github.com/giacomocavalieri/squirrel/issues/41
pub fn left_join_nullability_inference_test() {
  "
select
  users_issue41.user_id,
  profile_issue41.roles
from
  users_issue41
  left join profile_issue41
    on profile_issue41.user_id = users_issue41.user_id;
"
  |> should_codegen
  |> birdie.snap(title: "left join nullability inference")
}

// https://github.com/giacomocavalieri/squirrel/issues/43
pub fn nullability_with_foreign_key_condition_test() {
  "
select
  item_issue43.item_id,
  category_issue43.name
from
  item_issue43
  left join category_issue43 using(category_id)
where
  item_issue43.site_id = $1;
"
  |> should_codegen
  |> birdie.snap(title: "nullability with foreign key")
}

// https://github.com/giacomocavalieri/squirrel/issues/75
pub fn recursive_common_table_query_with_semi_join_test() {
  "
with recursive subcategories as (
  select id
  from categories_issue75
  where id = $1

  union all

  select c.id
  from categories_issue75 c
  join subcategories sc on c.parent_id = sc.id
)
select i.id, i.name
from items_issue75 i
left join items_categories_issue75 ic on ic.item_id = i.id
where ic.category_id in (select id from subcategories);"
  |> should_codegen
  |> birdie.snap(title: "recursive common table query with semi join")
}

pub fn squirrel_supports_do_blocks_test() {
  "
  do $$ begin
    select 1;
  end $$;
  "
  |> should_codegen
  |> birdie.snap(title: "squirrel supports do blocks")
}

pub fn file_with_squirrel_module_comment_is_considered_as_generated_test() {
  assert squirrel.LikelyGenerated
    == "select 1 as wibble"
    |> should_codegen_with_comments
    |> squirrel.classify_file_content
}

pub fn file_with_squirrel_function_comment_is_considered_as_generated_test() {
  assert squirrel.LikelyGenerated
    == "select 1 as wibble"
    |> should_codegen_with_comments
    // We remove the starting module comment, since we want to make sure
    // squirrel can pick up a generated file even if that bit is missing!
    |> string.split(on: "\n")
    |> list.filter(fn(line) { !string.starts_with(line, "////") })
    |> string.join(with: "\n")
    |> squirrel.classify_file_content
}

// If the length is 80 chars it's enough to generate a decoder with a comma in
// its empty args list, that's invalid syntax!
//
// https://github.com/giacomocavalieri/squirrel/issues/114
//
pub fn enum_with_a_long_enoug_name_does_not_generate_invalid_decoder_test() {
  "select 'wibble'::issue_114_aaaaaaaaaaaaaaaaa"
  |> should_codegen
  |> birdie.snap(
    title: "enum with a long enoug name does not generate invalid decoder",
  )
}
