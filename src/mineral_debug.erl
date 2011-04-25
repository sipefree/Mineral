%%%-------------------------------------------------------------------
%%% File    : mineral_debug.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Provides a logging function.
%%%-------------------------------------------------------------------

-module(mineral_debug).
-compile(export_all).

log(Text) ->
    log(Text, []).
log(Text, Replace) ->
    {{Year, Month, Day}, {Hour, Minutes, Seconds}} = erlang:localtime(),
    Formatted = io_lib:format(Text, Replace),
    io:format("[~p-~p-~p ~p:~p:~p] ~s~n", [Year,Month,Day,Hour,Minutes,Seconds,Formatted]).