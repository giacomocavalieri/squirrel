-module(squirrel_ffi).
-export([unique/0, exit/1, recover_string/1]).

recover_string(Binary) ->
    try
        Chars = lists:flatten(io_lib:format("~ts", [Binary])),
        {ok, unicode:characters_to_binary(Chars)}
    catch
        _ -> {error, nil}
    end.

unique() ->
    erlang:unique_integer([positive]).

exit(N) ->
    halt(N).
