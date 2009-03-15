-module(ftp_driver).
%-export([start/2, get_driver_pid/1, convert_command/1]).
-include("state.hrl").
-import(tcp, [crnl/0]).
-compile(export_all).

% network line terminator (terminates/seperates a ftp message from another).
% okay status flag.
-define(CRNL, "\r\n").
-define(R_OKAY, 200).


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
    send_reply(Socket, reply_code(service_ready), Hostname ++ " eFreeTPd server v0.1 waiting."),
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

    % FtpConnectionPid nachrichten senden mit empfangenen befehlen usw....
    % socket abfragen und umwandeln in eigenes format
    % anschließend an FtpConnectionPid senden und wieder warten (loopen)
%    debug:info("in ftp_driver/receive_loop ..."),
%    debug:info("current state: ~p", [State]),
    case tcp:get_request(Socket, Buf) of
	{ok, Request, Buf1} ->
	    case tcp:parse_request(Request) of
		{Command, Args} ->
		    debug:info("got request: ~p with params ~p", [Command, Args]),
		    % spawn handler-function for request:
		    FTPConnPid ! {command, Command, Args, State},
		    receive_loop(Socket, State, FTPConnPid, Buf1);
		error ->
		    send_reply(Socket, syntax_error, "syntax error: " ++ Request),
		    receive_loop(Socket, State, FTPConnPid, Buf1)
	    end;
	{error, closed} ->
	    debug:info("!! connection to ~p closed on port ~p:", [IP, Port]),
	    true
    end.



send_loop(Socket, State, ReceiveLoopPid) ->
    receive
	% auf nachrichten von send_loop in ftp_connection warten
	% und in ftp-protokoll-befehle umwandeln und über PortNr an client schicken
	{reply, Reply, Parameters, NewState} ->
	    
	    % inform ReceiveLoop of new state
	    ReceiveLoopPid ! {state_change, NewState},
	    
	    ReplyCode = reply_code(Reply),
	    % if there is a standard reply-string, then also send it
	    % otherwise simply send the replycode and the parameters (if there are any).
	    % also check the Parameters
	    % if non-empty list, join the parameters and send them as one string as well.
	    case reply_string(ReplyCode) of
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
    send_reply(Socket, reply_code(Replyname), Message);

send_reply(Socket, Code, Message) when is_integer(Code) ->
    gen_tcp:send(Socket, [integer_to_list(Code)," ",Message, ?CRNL]).


%% send a single standard message (reply-code only) to client.
send_reply(Socket, Replyname) when is_atom(Replyname) ->
    send_reply(Socket, reply_code(Replyname));

send_reply(Socket, Code) ->
    gen_tcp:send(Socket, [reply_string(Code)++ ?CRNL]).


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
		     ftp_command_mappings()),
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
		     ftp_command_mappings()),
    {FTP, _Internal} = hd(FilteredList),
    ftp_command_string(FTP).



%% returns a list of tuples containing the ftp protocoll command name (as an atom)
%% and the internally used name (as an atom) of each ftp command.
ftp_command_mappings() ->
    [{abor, abort_transfer},
     {cwd, change_work_dir},
     {dele, del_file},
     {list, list_files},
     {mdtm, get_modification_time},
     {nlst, name_list_remote_dir},
     {pass, password},
     {pasv, passive_mode},
     {port, open_data_port},
     {pwd, print_work_dir},
     {quit, quit_connection},
     {retr, retrieve_file},
     {rmd, remove_dir},
     {rnfr, rename_from},
     {rnto, rename_to},
     {site, site},
     {size, get_file_size},
     {stor, store_file},
     {type, transfer_type},
     {user, user_name},
     % from here on less common commands:
     % (as of: http://www.nsftools.com/tips/RawFTP.htm)
     {acct, account_info},
     {appe, append_to_file},
     {cdup, change_dir_up},
     {help, get_help},
     {mode, transfer_mode},
     {noop, noop},
     {rein, reinitialize_connection},
     {stat, get_server_status},
     {stou, store_file_uniquely},
     {stru, file_transfer_structure},
     {syst, get_system_type}].


reply_code(ReplyName) ->
    case ReplyName of
	mark_yyyy_mmmm -> 110;
	service_ready_in -> 120;
	data_connect_already_open -> 125;
	file_status_ok -> 150;
	command_ok -> 200;
	command_not_implemented -> 202;
	system_status -> 211;
	dir_status -> 212;
	file_status -> 213;
	help_message -> 214;
	name_system_type -> 215;
	service_ready -> 220;
	service_closing_control_connection -> 221;
	data_connection_open -> 225;
	closing_data_connection -> 226;
	entering_passive_mode -> 227;
	user_logged_in -> 230;
	req_file_action_ok -> 250;
	pathname_created -> 257;
	user_name_ok -> 331;
	need_account_for_login -> 332;
	req_file_action_pending -> 350;
	service_not_available -> 421;
	cant_open_data_connection -> 425;
	connection_closed -> 426;
	req_file_action_not_taken -> 450;
	req_action_error -> 451;
	req_action_not_taken -> 452;
	syntax_error -> 500;
	syntax_error_arguments -> 501;
	bad_seq_of_commands -> 503;
	command_not_implemented_for_param -> 504;
	not_logged_in -> 530;
	need_account_for_storing -> 532;
	req_action_aborted -> 551;
	req_file_action_aborted -> 552
    end.


%% Standard reply strings and theier meaning
reply_string(110) -> "110 MARK yyyy = mmmm";             %% ARGS
reply_string(120) -> "120 Service ready in nnn minutes.";  %% ARG
reply_string(125) -> "125 Data connection already open; transfere starting.";
reply_string(150) -> "150 File status okay; about to open data connection.";
reply_string(200) -> "200 Command okay.";
reply_string(202) -> "202 Command not implemented, superfluos at this site.";
reply_string(211) -> "211 System status, or system help reply.";
reply_string(212) -> "212 Directory status.";
reply_string(213) -> "213 File status.";
reply_string(214) -> "214 Help message.";     %% ADD HELP
reply_string(215) -> "215 NAME system type";  %% set NAME
reply_string(220) -> "220 Service ready for user.";
reply_string(221) -> "221 Service closing control connection.";
reply_string(225) -> "225 Data connection open; no transfere in progress";    
reply_string(226) -> "226 Closing data connection.";  %% ADD INFO
reply_string(227) -> "227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).";  %% ARGS
reply_string(230) -> "230 User logged in, proceed.";
reply_string(250) -> "250 Requested file action okay, completed.";
reply_string(257) -> "257 PATHNAME created.";  %% ARG
reply_string(331) -> "331 User name okay, need password.";
reply_string(332) -> "332 Need account for login.";
reply_string(350) -> "350 Requested file action pending further information.";
reply_string(421) -> "421 Service not available, closing control connection.";
reply_string(425) -> "425 Can't open data connection.";
reply_string(426) -> "426 Connection closed; transfere aborted.";
reply_string(450) -> "450 Requested file action not taken.";
reply_string(451) -> "451 Requested action not taken: local error in processing.";
reply_string(452) -> "452 Requested action not taken.";
reply_string(500) -> "500 Syntax error, command unrecognized.";  %% ADD INFO
reply_string(501) -> "501 Syntax error in paramters or arguments.";
reply_string(502) -> "502 Command not implemented.";
reply_string(503) -> "503 Bad sequence of commands.";
reply_string(504) -> "504 Command not implemented for that parameter.";
reply_string(530) -> "530 Not logged in.";
reply_string(532) -> "532 Need account for storing files.";
reply_string(550) -> "550 Requested action not taken.";
reply_string(551) -> "551 Requested action aborted: page type unkown.";
reply_string(552) -> "552 Requested file action aborted.";
reply_string(553) -> "553 Requested action not taken.";
reply_string(_) -> not_available.



%% returns a ftp-command string as an atom.
-spec ftp_command_atom(string()) -> atom().

ftp_command_atom(CommandString) ->
    erlang:list_to_atom(string:to_lower(CommandString)).


%% reverse of ftp_command_atom/1.
%% returns a ftp-command string based on a given command-atom.
-spec ftp_command_string(atom()) -> string().

ftp_command_string(CommandAtom) ->
    string:to_upper(erlang:atom_to_list(CommandAtom)).


%to_ftp_packet({reply, {
to_ftp_packet(UnknownMessage) ->
    debug:error("don't know how to convert this message to ftp-packet: ~p~n",
		[UnknownMessage]).


get_driver_pid(PortNr) ->
    ProcName = utils:process_name("ftp_driver_", PortNr),
    whereis(ProcName).
