%%%-------------------------------------------------------------------
%%% File    : mineral_client.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Client process.
%%%-------------------------------------------------------------------
-module(mineral_client).

-include ("mineral.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).
-export([new/0]).

-record(state, {uid=0, sock=false, chid=0}).

% generates a new random connection-id based on some various random properties
	
%% new() -> pid()
new() ->
	process_flag(trap_exit, true),
    State = #state{},
	proc_lib:spawn_link(fun() -> loop(State) end).

% loop() is the main receive loop for the worker process.
% receives messages from three sources:
% gen: both client and server can request these
% cli: message from the client
% srv: message from the server
%
% some messages are async. the worker may receive a message from the client,
% and then call one of the server's methods, but it will not send a reply
% to the socket. the server may send messages to the requesting client
% during its operation, but the worker is oblivious to this and passes
% along these messages as if they were anything else
%
% other messages are synchronous. the worker receives a message from the client,
% calls the server for a piece of information, then immediately sends a message
% back to the client with the information received.
%
% messages from srv are usually messages of an info nature, such as the status
% of other players, etc. these messages are put into a standard format by the worker
% and sent to the socket.

%% loop(State) -> recursive
%%   State = record(state)
loop(#state{sock = Sock} = State) ->
	% this loop always creates a new state called NewState which may or may not be changed.
	receive
		{gen, getuid, From} ->
			NewState = State,
			From ! {uid, State#state.uid, self()};
		
		{gen, destroy, _From} ->
			exit(userdisconnect);
			
		{cli, {setsock, ASock}, _From} ->
			NewState = State#state{sock=ASock};
			
		{srv, {setuid, Uid}, _From} ->
			NewState = State#state{Uid=Uid};
		
		{srv, force_disconnect, _From} ->
			NewState = State,
			Sock ! {msg, disconnect},
			self() ! {gen, destroy, self()};
		
		{cli, ok, _From} ->
			mineral_debug:log("Client is ready: ~p", [State#state.uid]),
			NewState = State;
		
		{cli, keepalive, _From} ->
			NewState = State,
			Sock ! {msg, keepalive};
		
		OtherMsg ->
			NewState = State,
			mineral_debug:log("mineral_client ~p received unknown msg: ~p", [self(), OtherMsg])
	end,
	loop(NewState).