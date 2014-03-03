-module(meminfo).

-export([info/0]).

%%--------------------------------------------------------------------
%% @doc Read /proc/meminfo
%%
%% @end
%%--------------------------------------------------------------------

-spec info() -> [{atom(), integer()}].
info() ->
    {ok, F} = file:open("/proc/meminfo", [read]),
    {ok, Contents} = file:read(F, 4096),
    file:close(F),
    Tokenized = string:tokens(Contents, ": \n"),
    process(Tokenized, []).

process(["MemTotal", T, "kB" | Rest], Result) ->
    process(Rest, [{mem_total, list_to_integer(T)} | Result]);
process(["MemFree", T, "kB" | Rest], Result) ->
    process(Rest, [{mem_free, list_to_integer(T)} | Result]);
process([_X, "0" | Rest], Result) ->
    process(Rest, Result);
process([_X, _Y, _Z | Rest], Result) ->
    process(Rest, Result);
process([], Result) ->
    Result.
