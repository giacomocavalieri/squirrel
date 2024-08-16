-module(squirrel_ffi).
-export([unique/0, exit/1]).

unique() ->
    erlang:unique_integer([positive]).

exit(N) ->
    halt(N).
