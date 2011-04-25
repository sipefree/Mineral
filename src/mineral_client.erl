%%%-------------------------------------------------------------------
%%% File    : mineral_client.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Client process.
%%%-------------------------------------------------------------------
-module(mineral_client).

-include ("mineral.hrl").
-include_lib("kernel/include/file.hrl").
-include("../include/client_packets.hrl").
-include("../include/server_packets.hrl").
-compile(export_all).
-export([new/0]).

-record(state, {uid="", sock=false, eid=0}).
    
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
            From ! {uid, State#state.uid, self()},
            loop(State);
        
        {gen, destroy, _From} ->
            exit(userdisconnect);
            
        {cli, {setsock, ASock}, _From} ->
            mineral_server:client_ready(),
            loop(State#state{sock=ASock});
        
        {srv, force_disconnect, _From} ->
            Sock ! {msg, disconnect},
            self() ! {gen, destroy, self()},
            loop(State);
        
        {cli, ok, _From} ->
            loop(State);
        
        {cli, #cli_keep_alive{}, _From} ->
            Sock ! #srv_keep_alive{},
            loop(State);
            
        {cli, #cli_login_request{protocol_version = ProtoVer, username = Username}, _From} ->
            mineral_debug:log("[LOGIN] Client login request received: ~p", Username),
            Sock ! mineral_server:login_request(ProtoVer, Username),
            loop(State);
        
        {cli, #cli_handshake{username = Username}, _From} ->
            mineral_server:handshake(Username),
            loop(State);
        
        {cli, #cli_chat_message{message = Message}, _From} ->
            minseral_server:chat_message(Message),
            loop(State);
            
        
        
        
        {srv, Other, _From} ->
            Sock ! Other,
            loop(State);
        
        OtherMsg ->
            mineral_debug:log("[CLIENT] Client ~p received unknown msg: ~p", [self(), OtherMsg]),
            loop(State)
    end,
    loop(State).