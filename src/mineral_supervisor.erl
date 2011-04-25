%%%-------------------------------------------------------------------
%%% File    : mineral_supervisor.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Supervises the required servers.
%%%-------------------------------------------------------------------

-module(mineral_supervisor).
-behaviour(supervisor).

-export([start/0, start_debug/0, start_link/1, init/1]).

start() ->
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg=[])
    end).

start_debug() ->
    mineral_debug:log("Starting Mineral Supervisor in shell..."),
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    mineral_debug:log("Mineral supervisor starting..."),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    {ok, 
        {
            {one_for_one, 1, 60},
              [
                {mineral_server, % game server
                    {mineral_server, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [mineral_server]},
                {mineral_sockserver,
                    {mineral_sockserver, start_link, [25565]},
                    permanent,
                    brutal_kill,
                    worker,
                    [mineral_sockserver]}
            ]
        }
    }.