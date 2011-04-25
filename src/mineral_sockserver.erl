%%%-------------------------------------------------------------------
%%% File    : mineral_sockserver.erl
%%% Author  : Simon Free <sipefree@gmail.com>
%%% Description : Accepts connections and spawns socket processes for each.
%%%-------------------------------------------------------------------

-module(mineral_sockserver).

-behaviour(gen_server).

-export([start_link/1, create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listen_socket,
                port,
                acceptor}).

start_link(Port) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("mineral_sockserver_~w", [Port]))),
    gen_server:start_link({local, Name}, ?MODULE, Port, []).

create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).


init(Port) ->	
    process_flag(trap_exit, true),
	mineral_debug:log("Mineral Socket Server (~p) starting on port ~p...", [?MODULE, Port]),
    case gen_tcp:listen(Port,[binary, {packet, 4},
                              {reuseaddr, true},
                              {active, false},
                              {backlog, 1024},
							  {keepalive, true},
							  {nodelay, true}]) of
	{ok, Listen_socket} ->
		mineral_debug:log("Socket process will listen for new connections..."),
	    Pid = mineral_socket:start_link(self(), Listen_socket, Port),
	    {ok, #state{listen_socket = Listen_socket,
                        port = Port,
						acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({create, _Pid}, #state{listen_socket = Listen_socket} = State) ->
	mineral_debug:log("Socket process will handle client..."),
    New_pid = mineral_socket:start_link(self(), Listen_socket, State#state.port),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
	mineral_debug:log("Mineral Socket Server: Connection closed"),
    {noreply, State};

handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(2000),
    mineral_socket:start_link(self(), State#state.listen_socket, State#state.port),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
