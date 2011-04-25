%%%-------------------------------------------------------------------
%%% File    : mineral_app.hrl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Helper for erlang application API.
%%%-------------------------------------------------------------------

-module(mineral_app).
-behaviour(application).
-export([start/2, stop/1]).

% OTP app

start(_Type, StartArgs) ->
	io:format("~n"),
	mineral_debug:log("Mineral Minecraft Server starting..."),
	mineral_supervisor:start_link(StartArgs).

stop(_State) ->
	ok.