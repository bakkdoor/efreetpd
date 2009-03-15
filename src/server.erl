%% listening-server module.
%% listens on a given port (default: 21) for incoming connections
%% and starts a ftp_connection process for each client. 

-module(server).
-export([start/1, stop/0]).
-import(ftp_driver, [reply_code/1, reply_string/1, send_reply/2, send_reply/3]).
-import(tcp, [get_request/2, parse_request/1, parse_address/1, format_address/1, format_address/2, alpha/1]).
-include("state.hrl").
-authors([{"Christopher Bertels", "bakkdoor@flasht.de"},
	  {"Denis Meyer", "calltopower88@web.de"}]).


%% starts the server's main (listening) loop on a given port (standard is 21 for ftp). 
start(ListenPort) ->
    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {active, false}, {packet, 0}]),
    debug:info("server startet on port ~p", [ListenPort]),
    AcceptLoop = spawn(fun() -> accept_loop(LSocket) end),
    ServerStopperPid = spawn(fun() ->
			      put(lsocket, LSocket),
			      receive
				  stop ->
				      gen_tcp:close(get(lsocket))
			      end
		      end),
    register(server, ServerStopperPid),
    AcceptLoop.
    

%% stops the server's main (listening) loop.
stop() ->
    server ! stop.
%    unregister(server).


%% loop for incoming connections.
%% listens on LSocket and accepts new connections, giving each an individual port
%% to communicate over.				   
accept_loop(LSocket) ->				   
    case gen_tcp:accept(LSocket) of

	{ok, NewSocket} -> 
	    debug:info("new connection on socket ~p (~p)", [NewSocket, inet:peername(NewSocket)]),
	    spawn(fun() -> accept_loop(LSocket) end),
	    start_connection(NewSocket);

	{error, closed} ->
	    debug:info("accept_loop failed: listening socket closed."),
	    debug:info("stopping...");

	{error, Why} ->
	    debug:error("accept_loop failed: ~p", [Why]),
	    debug:error("stopping...");
	
	stop ->
	    gen_tcp:close(LSocket),
	    debug:info("stopping server...")
    end.


% starts the ftp_connection process for a given socket.
start_connection(Socket) ->
    % initial state
    {ok, {IP, Port}} = inet:peername(Socket),
    State = #state{ip = IP, port = Port},
    debug:info("starting ftp_connection/start for client:  ~p:~p", [IP, Port]),
    ftp_connection:start(Socket, State).
