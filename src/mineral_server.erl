%%%-------------------------------------------------------------------
%%% File    : mineral_server.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Main Server Process.
%%%-------------------------------------------------------------------
-module(mineral_server).

-behaviour(gen_server).

-include("mineral.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-compile(export_all).

-record(state, {clients}). % clients = [{Cid, mineral_client_process(Pid)}]

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mineral_server}, ?MODULE, [], []).





%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	mineral_debug:log("Mineral Server (~p) starting...", [?MODULE]),
    {ok, #state{clients=[]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	mineral_debug:log("Mineral Server received instruction to code change. Nothing to do.", []),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_client(_, []) -> noclient;
get_client(Uid, [Pid|T]) ->
	io:format("Asking ~p for UID~n", [Pid]),
	Pid ! {gen, getuid, self()},
	receive
		{uid, AUid, Pid} ->
			case AUid of
				Uid ->
					{pid, Pid};
				_ ->
					get_client(Uid, T)
			end;
		_ ->
			get_client(Uid, T)
	end.
