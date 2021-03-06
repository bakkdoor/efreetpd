%% Main module for ftp_connection process.
%% Handles an ftp connection to a connected client.
%% Starts a ftp_driver process, that talks to the client over a socket.
%% Also starts subprocesses for a given ftp command (e.g. uploading files, creating dirs etc.)
%% if requested by ftp_driver process. 

-module(ftp_connection).
-export([start/2, get_connection_pid/1]).
-include("state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


start(Socket, State) ->
    % start loop & ftp_driver processes
    LoopPid = spawn(fun () -> receive_loop(State) end),
    Port = State#state.port,
    register(utils:process_name(Port), LoopPid),
    start_driver(Socket, State, LoopPid).


%% main loop, waiting for messages by the ftp_driver process.
%% if a command request comes in, we start a subprocess for the requested command.
receive_loop(State) ->
    % loop as long as connection is alive
    receive
	% the ftp_driver has closed the connection to the client on this port.
	{quit, PortNr} ->
	    debug:info("connection closed on port: ~w~n", [PortNr]);

	% ftp_driver has told us its send_loop process, so we start our own
	{send_loop_pid, DriverSendLoop} ->
	    SendLoopPid = spawn(fun() -> send_loop(DriverSendLoop) end),
	    put(send_loop_pid, SendLoopPid),
	    receive_loop(State);

	% command request to be executed
	% start a subprocess to deal with the command
	{command, Command, Parameter, CmdState} ->
	    debug:info("executing command: ~p with args: ~p", [Command,  Parameter]),
	    execute_command(Command, Parameter, CmdState),
	    receive_loop(CmdState);

	{state_change, NewState} ->
	    receive_loop(NewState);

	{reply, Reply, Parameters, NewState} ->
	    debug:info("sending reply: ~p (~p) to client...", [Reply, Parameters]),
	    receive_loop(NewState);
	
	% any other messages, we simply output and keep on going...
	Unknown ->
	    debug:info("unknown command: ~w~n", [Unknown]),
	    receive_loop(State)
    end.


%% loop for sending requests to the client via ftp_driver process.
%% sends messages to client via DriverSendLoop (ftp_driver:send_loop) with data/commands/replies.
send_loop(DriverSendLoop) ->
    % wait for messages to forward to ftp_driver
    receive
	Reply = {reply, Reply2} ->
	    case Reply2 of
		{data_stream, _} ->
		    debug:info("sending data to client..."), % just for debugging...
		    DriverSendLoop ! Reply,
		    send_loop(DriverSendLoop);
		
		{message, Message} ->
		    debug:info("sending message to client: ~p~n", [Message]),
		    DriverSendLoop ! Reply,
		    send_loop(DriverSendLoop);

		{dir_listing, Dir, Listing} ->
		    debug:info("listing directory contents of dir ~p: ~p~n", [Dir, Listing]),
		    DriverSendLoop ! Reply,
		    send_loop(DriverSendLoop);

		{file_deleted, File} ->
		    debug:info("deleted file ~p~n", [File]),
		    DriverSendLoop ! Reply,
		    send_loop(DriverSendLoop);
		
	        UnknownReply ->
		    debug:info("error: unknown reply format: ~p~n", [UnknownReply]),
		    send_loop(DriverSendLoop)
	    end;
	
	NotImplemented = {reply, cmd_not_implemented, _Commandname} ->
	    DriverSendLoop ! NotImplemented,
	    send_loop(DriverSendLoop);

	Reply = {reply, _Replyname, _Params, _State} ->
	    DriverSendLoop ! Reply,
	    send_loop(DriverSendLoop);

	Request = {request, data_connection, _FromPid, _State} ->
	    debug:info("ftp_connection/send_loop: requesting data_connection process"),
	    DriverSendLoop ! Request;

	Unknown ->
	    debug:error("ftp_connection/send_loop: unkown message in send_loop (# ~p): ~p~n", [self(), Unknown]),
	    send_loop(DriverSendLoop)
    end.


% starts a new process for a given command.
execute_command(Command, Parameters, State) ->
    SendLoopPid = get(send_loop_pid),
    ParamsWithPid =  [SendLoopPid, State | [Parameters]],
    spawn(Command, start, ParamsWithPid).


start_driver(Socket, State, LoopPid) ->
    ftp_driver:start(Socket, State, LoopPid).


%% gets called as exit handler for crashing ftp_driver processes
driver_receive_loop_exit_handler(Pid, ExitReason) ->
    debug:error("ftp_driver receive_loop (~p) crashed: ~p~n", [Pid, ExitReason]),
    
    case ExitReason of
	{error, Message, Socket, State} ->
	    Port = State#state.port,
	    debug:error("~w crashed on port: ~p~nMessage: ~p~n", ["\t\t",Port, Message]),	    
	    case get_connection_pid(Port) of
		undefined ->
		    io:format("error: ftp_connection process seems to be dead on port: ~p~n", [Port]),
		    exit("something went very wront here.");
		LoopPid ->
		    start_driver(Socket, State, LoopPid)
	    end;
	
	Unknown ->
	    debug:error
	      ("~w unknown error: ~p", ["\t\t",Unknown])
    end.


%% returns Pid for ftp_connection process tied to a given port.
get_connection_pid(PortNr) ->
    ProcName = utils:process_name(PortNr),
    whereis(ProcName).
