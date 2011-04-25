-module(mineral_socket).

-author(sipefree@gmail.com).

-compile(export_all). 
-include("mineral.hrl").

-record(c,  {sock,
             port,
             peer_addr,
             peer_port
         }).

-record(state, {c, worker}).

create_message(Instr, Data) ->
    mineral_debug:log("Packing message, Instr:~p Data:~p", [Instr, Data]),
    Bin = mineral_msg:instr_pack(Instr, Data),
    <<Instr:8,Bin/bytes>>.

start_link(ListenPid, ListenSocket, ListenPort) ->
    proc_lib:spawn_link(?MODULE, init, [{ListenPid, ListenSocket, ListenPort}]).

init({Listen_pid, Listen_socket, ListenPort}) ->
    mineral_debug:log("[CONNECT] Mineral Socket (~p) starting...", [?MODULE]),
    case catch gen_tcp:accept(Listen_socket) of
        {ok, Socket} ->
            mineral_sockserver:create(Listen_pid, self()),
            {ok, {Addr, Port}} = inet:peername(Socket),
            C = #c{sock = Socket,
                   port = ListenPort,
                   peer_addr = Addr,
                   peer_port = Port},
            mineral_debug:log("[CONNECT] Mineral Socket accepted socket ~s", [ip_format(inet:peername(Socket))]),
            mineral_debug:log("[CONNECT] Mineral Socket beginning receive loop...", []),
            IPStr = lists:flatten(io_lib:format("recv_~s", [ip_to_string(inet:peername(Socket))])),
            RegAtom = list_to_atom(IPStr),
            register(RegAtom, self()),
            recv_loop(#state{c=C, worker=false});
        Else ->
            error_logger:error_report([{application, mineral},
                           "[FAIL/CONNECT] Accept failed error",
                           io_lib:format("~p",[Else])]),
            exit({error, accept_failed})
    end.

send_message(#c{sock = Socket}, Msg) ->
    %io:format("sending ~p~n", [Msg]),
    case gen_tcp:send(Socket, Msg) of
        {error, Err} ->
            self() ! {sendfail, Err};
        _ ->
            ok
    end.

disconnect(#c{sock = Socket}) ->
    case gen_tcp:close(Socket) of
        {error, Reason} ->
            mineral_debug:log("[FAIL/DISCONNECT] Failed to close a socket because ~p", [Reason]);
        ok ->
            ok
    end.

recv_loop(#state{c=C, worker=W}) ->
    case W of
        false ->
            Pid = mineral_server:new_client(),
            OutgoingPid = spawn_link(fun() -> handle_outgoing(C) end),
            register(list_to_atom(lists:flatten(io_lib:format("out_~s", [ip_to_string(inet:peername(C#c.sock))]))), OutgoingPid),
            Pid ! {cli, {setsock, OutgoingPid}, self()},
            Pid ! {cli, ok, self()},
            recv_loop(#state{c=C, worker=Pid});
        _ ->
            case gen_tcp:recv(C#c.sock, 0) of
                {ok, Bin} ->
                    Msg = mineral_msg:instr_unpack(Bin),
                    case W of
                        false ->
                            mineral_debug:log("[FAIL] Mineral Socket received message but has no worker!"),
                            exit(normal);
                        Pid ->
                            Pid ! Msg
                    end;
                _Other ->
                    case W of
                        false ->
                            mineral_debug:log("[DISCONNECT] Mineral Socket has disconnected without a worker process."),
                            exit(normal);
                        Pid ->
                            mineral_debug:log("[DISCONNECT] Mineral Socket has disconnected, shutting down worker."),
                            mineral_server:client_disconnect(Pid),
                            exit(normal)
                    end
            end,
            recv_loop(#state{c=C, worker=W})
    end.
    
handle_outgoing(C) ->
    receive

        Msg ->
            mineral_debug:log("[FAIL/SEND] Mineral Socket cannot send message: ~p", [Msg])
    end,
    handle_outgoing(C).

ip_to_string({ok, {{A, B, C, D}, Port}}) ->
    io_lib:format("~p_~p_~p_~p_~p", [A, B, C, D, Port]).

ip_format({ok, {{A, B, C, D}, Port}}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).