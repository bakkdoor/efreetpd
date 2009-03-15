%% ftp_driver-module.
%% here all the sending & receiving of binary messages via sockets takes place.
%% messages received over the network get converted into internal messages
%% and sent to the ftp_connection process of each connected client.
%% at the same time, reply-messages ordered to be sent back to the client
%% are received from the ftp_connection process, converted to an appropriate
%% ftp-protocol conform binary message and sent over the network.

-module(ftp_driver).
%-export([start/2, get_driver_pid/1, convert_command/1]).
-compile(export_all).
-include("state.hrl").
-import(tcp, [crnl/0]).

% network line terminator (terminates/seperates a ftp message from another).
-define(CRNL, "\r\n").


start(Socket, State, FtpConnectionPid) ->
    case gen_tcp:controlling_process(Socket, self()) of
	ok ->
	    NewState = acknowledge(Socket, State),
	    EmptyBuffer = [],
	    
	    ReceivePid = self(),

	    SendLoop = spawn(fun() -> send_loop(Socket, NewState, ReceivePid) end),
	    FtpConnectionPid ! {send_loop_pid, SendLoop},

	    receive_loop(Socket, NewState, FtpConnectionPid, EmptyBuffer);
	Error ->
	    debug:error("could not set controlling_process: ~p", [Error])
    end.
    

%% send acknowledge packet to client.
acknowledge(Socket, State) ->
    {ok, Hostname} = inet:gethostname(),
    IP = State#state.ip, 
    Port = State#state.port,
    debug:info("sending acknowledge message to client (# ~p:~p)", [IP, Port]),
    send_reply(Socket, service_ready, 
	       Hostname ++ " eFreeTPd server v0.1 waiting."),
    RootDir = config:setting(root_dir),
    % return new state
    State#state{current_dir = RootDir, data_port = Port - 1}.


receive_loop(Socket, State = #state{ip = IP, port = Port}, FTPConnPid, Buf) ->
    
    % if we get a state_change message, 
    % change the state by making the recursive call with the new state.
    % after 0.1 sec simply move on
    receive
	
	{state_change, NewState} ->
	    debug:info("--------> ftp_driver/receive_loop: got state_change: ~p", [NewState]),
	    receive_loop(Socket, NewState, FTPConnPid, Buf);
	
	Other ->
	    debug:info("ftp_driver/receive_loop: unkown message: ~p", [Other])

	after 100 ->
		move_on
    end,

    % process received requests and send as command-messages to FtpConnectionPid,
    % then loop again.

%    debug:info("in ftp_driver/receive_loop ..."),
%    debug:info("current state: ~p", [State]),

    case tcp:get_request(Socket, Buf) of
	{ok, Request, Buf1} ->
	    case tcp:parse_request(Request) of
		{Command, Args} ->
		    process_command(FTPConnPid, Socket, State, Command, Args),
		    receive_loop(Socket,  State, FTPConnPid, Buf1);
		error ->
		    send_reply(Socket, syntax_error, "syntax error: " ++ Request),
		    receive_loop(Socket, State, FTPConnPid, Buf1)
	    end;
	{error, closed} ->
	    debug:info("!! connection to ~p closed on port ~p:", [IP, Port]),
	    true
    end.


%% if no special command given, simply forward to FTPConnPid,
%% if special, deal with it here. 
process_command(FTPConnPid, Socket, State, Command, Args) ->
    case Command of
	% passive_mode needs to be dealt here 
	% some low-level operations are required, which we want to take care of
	% only within this module.
	passive_mode ->
	    passive_mode(Socket, State);

	open_data_port ->
	    open_data_port(Socket, State, Args);
	
	% any other command simply gets forwarded outside
	% to the ftp_connection process.
	_Other ->
	    debug:info("got request: ~p with params ~p", [Command, Args]),
	    % spawn handler-function for request:
	    FTPConnPid ! {command, Command, Args, State}
    end.


%% receives messages from ftp_connection process, converts them
%% and finally sends them as binary messages to the client.
send_loop(Socket, State, ReceiveLoopPid) ->
    receive
	{reply, cmd_not_implemented, _Commandname} ->
	    cmd_not_implemented(Socket),
	    send_loop(Socket, State, ReceiveLoopPid);

	{reply, {dir_listing, _Dir, Listing}} ->
	    lists:foreach(fun(File) ->
				  gen_tcp:send(Socket, File ++ ?CRNL)
			  end,
			  Listing),
	    send_reply(Socket, closing_data_connection);
	    
	% auf nachrichten von send_loop in ftp_connection warten
	% und in ftp-protokoll-befehle umwandeln und über PortNr an client schicken
	{reply, Reply, Parameters, NewState} ->
	    
	    % inform ReceiveLoop of new state
	    ReceiveLoopPid ! {state_change, NewState},
	    
	    ReplyCode = ftp_protocol:reply_code(Reply),
	    % if there is a standard reply-string, then also send it
	    % otherwise simply send the replycode and the parameters (if there are any).
	    % also check the Parameters
	    % if non-empty list, join the parameters and send them as one string as well.
	    case ftp_protocol:reply_string(ReplyCode) of
		not_available ->
		    case Parameters of
			[] ->
			    send_reply(Socket, ReplyCode);
			List = [_|_] -> 
			    % TODO: check, if space is correct seperator (maybe CRNL ?)
			    ParamsString = string:join(List, " "),
			    send_reply(Socket, ReplyCode, ParamsString)
		    end;
		ReplyMessage ->
		    case Parameters of
			[] ->
			    send_reply(Socket, ReplyCode);
			List = [_|_] ->
			    % TODO: same here...
			    ParamsString = string:join(List, " "),
			    send_reply(Socket, ReplyCode, ParamsString ++ " " ++ ReplyMessage)
		    end	    
	    end,
	    send_loop(Socket, NewState, ReceiveLoopPid);
	
	{quit, Socket, _NewState = #state{port = Port}} ->
	    debug:info("FTPDriver for connection on port ~w quitting.", [Port]);

	Unknown ->
	    debug:info("unknown command received in ftp_driver: ~w", [Unknown]),
	    send_loop(Socket, State, ReceiveLoopPid)
    end.


%% send a reply-message (network-line-terminated status code and a message)
%% to the client (via Socket).
send_reply(Socket, Replyname, Message) when is_atom(Replyname) ->
    send_reply(Socket, ftp_protocol:reply_code(Replyname), Message);

send_reply(Socket, Code, Message) when is_integer(Code) ->
    gen_tcp:send(Socket, [integer_to_list(Code)," ",Message, ?CRNL]).


%% send a single standard message (reply-code only) to client.
send_reply(Socket, Replyname) when is_atom(Replyname) ->
    send_reply(Socket, ftp_protocol:reply_code(Replyname));

send_reply(Socket, Code) ->
    gen_tcp:send(Socket, [ftp_protocol:reply_string(Code)++ ?CRNL]).


-spec convert_command(string(), list()) -> tuple().

convert_command(FTPCommandString, Parameters) ->
    {convert_command(FTPCommandString), Parameters}.
    


%% returns internal command-names for each ftp-protocoll command name.
%% e.g.: 
%% "ABOR" -> abort_transfer,
%% "CWD" -> change_work_dir,
%% "RNTO" -> rename_to.
%% etc...
-spec convert_command(string() | atom()) -> atom() | string().

convert_command(FTPCommandString) when erlang:is_list(FTPCommandString) ->
    % filter all mappings, where the FTPCommand-Atom 
    % (first entry in each tuple in ftp_command_mappings)
    % is the same as FTPCommandString
    FilteredList = 
	lists:filter(fun(Mapping) -> 
			     case Mapping of
				 {FTP, _Internal} ->
				     ftp_command_atom(FTPCommandString) =:= FTP
			     end
		     end,
		     ftp_protocol:ftp_command_mappings()),
    % get the first element of list (it actually should only contain 1 element)
    {_FTP, Internal} = hd(FilteredList),
    % and return the internal command atom.
    Internal;
	    
convert_command(CommandAtom) when erlang:is_atom(CommandAtom) ->
    %% this version does the reverse of the first clause:
    FilteredList =
	lists:filter(fun(Mapping) ->
			     case Mapping of
				 {_FTP, Internal} ->
				     CommandAtom =:= Internal
			     end
		     end,
		     ftp_protocol:ftp_command_mappings()),
    {FTP, _Internal} = hd(FilteredList),
    ftp_protocol:ftp_command_string(FTP).



%% command not implemented
cmd_not_implemented(Socket) ->
    send_reply(Socket, 502).


%% returns a ftp-command string as an atom.
-spec ftp_command_atom(string()) -> atom().

ftp_command_atom(CommandString) ->
    erlang:list_to_atom(string:to_lower(CommandString)).


%% reverse of ftp_command_atom/1.
%% returns a ftp-command string based on a given command-atom.
-spec ftp_command_string(atom()) -> string().

ftp_command_string(CommandAtom) ->
    string:to_upper(erlang:atom_to_list(CommandAtom)).


get_driver_pid(PortNr) ->
    ProcName = utils:process_name("ftp_driver_", PortNr),
    whereis(ProcName).



%%%%%%%%%%%%%%%%%% special commands %%%%%%%%%%%%%%%%%%

passive_mode(Socket, State) ->
    user:assert_logged_in(Socket, State,
			 fun() ->
				 St1 = tcp:close_listen(State),
				 {ok,{Addr,_}} = inet:sockname(Socket),
				 case gen_tcp:listen(0, [{active,false}, binary]) of
				     {ok,ListenSock} ->
					 {ok,{_,Port}} = inet:sockname(ListenSock),
					 send_reply(Socket, entering_passive_mode, "Entering Passive Mode (" ++
						    tcp:format_address(Addr,Port) ++ ")."),
					 St1#state { listen_socket = ListenSock };
				     {error,Err} ->
					 send_reply(Socket, cant_open_data_connection, erl_posix_msg:message(Err)),
					 St1
				 end
			 end).


open_data_port(Socket, State, Args) ->
    user:assert_logged_in(Socket, State,
			  fun() ->
				  St1 = tcp:close_listen(State),
				  {ok,AddrPort} = tcp:parse_address(Args),
				  send_reply(Socket, command_ok),
				  St1#state { data_port = AddrPort, listen_socket = undefined }
			  end).
