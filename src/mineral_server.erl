%%%-------------------------------------------------------------------
%%% File    : mineral_server.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Main Server Process.
%%%-------------------------------------------------------------------
-module(mineral_server).

-behaviour(gen_server).

-include("mineral.hrl").
-include("../include/client_packets.hrl").
-include("../include/server_packets.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-compile(export_all).

% clients = [{Cid, mineral_client_process(Pid)}]
-record(entity, {eid, entity}).
-record(state, {clients, entities, next_eid=1}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mineral_server}, ?MODULE, [], []).

new_client() ->
    gen_server:call(?MODULE, newclient, 20000).

client_ready() ->
    gen_server:call(?MODULE, client_ready, 20000).

client_disconnect(Pid) ->
    gen_server:call(?MODULE, {client_disconnect, Pid}, 20000).
    
login_request(ProtoVer, Username) ->
    gen_server:call(?MODULE, {login_request, ProtoVer, Username}, 20000).

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

% spawns a client process, returns process id
handle_call(newclient, _From, State) ->
    mineral_debug:log("[SERVER] Starting new client worker, total clients: ~p", [length(State#state.clients)+1]),
    Pid = mineral_client:new(),
    NewState = State#state{clients=[Pid|State#state.clients]},
    {reply, Pid, NewState};

handle_call(client_ready, {Pid, _Tag}, State) ->
    Pid ! {srv, #srv_handshake{connection_hash="-"}, self()},
    {reply, true, State};

handle_call({login_request, ProtoVer, _Username}, _From, State) ->
    case ProtoVer of
        ?MINERAL_PROTOCOL_VERSION ->
            {
                reply,
                #srv_login_response{
                    player_entity_id = State#state.next_eid,
                    unused_string = "derp",
                    map_seed = 1283198273, % FIXME: real map seed
                    dimension = 0
                    },
                State#state{next_eid = State#state.next_eid + 1}
            };
        _ ->
            Reason = io_lib:format("Incompatible protocol: ~p (we're on ~p).", [ProtoVer, ?MINERAL_PROTOCOL_VERSION]),
            {
                reply,
                #srv_disconnect{ reason=Reason },
                State
            }
    end;

handle_call({client_disconnect, Pid}, _From, State) ->
    Pid ! {gen, destroy, self()},
    {reply, true, State#state{clients=lists:delete(Pid, State#state.clients)}}.

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
