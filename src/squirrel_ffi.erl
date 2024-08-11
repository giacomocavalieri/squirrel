-module(squirrel_ffi).
-export([unique/0]).

unique() ->
    erlang:unique_integer([positive]).
