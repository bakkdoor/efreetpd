-module(ftp_driver).
%-export([start/2, get_driver_pid/1, convert_command/1]).
-compile(export_all).

start(PortNr, FtpConnectionPid) ->
    % socket oeffnen auf port PortNr
    % und loopen
    _Receiveloop = spawn(fun() -> receive_loop(PortNr, FtpConnectionPid) end),
    SendLoop = spawn(fun() -> send_loop(PortNr) end),
    FtpConnectionPid ! {send_loop_pid, SendLoop, PortNr}.

receive_loop(PortNr, FtpConnectionPid) ->
    % FtpConnectionPid nachrichten senden mit empfangenen befehlen usw....
    % socket abfragen und umwandeln in eigenes format
    % anschließend an FtpConnectionPid senden und wieder warten (loopen)
    
    receive_loop(PortNr, FtpConnectionPid).


send_loop(PortNr) ->
    receive
	% auf nachrichten von send_loop in ftp_connection warten
	% und in ftp-protokoll-befehle umwandeln und über PortNr an client schicken
	{command, Command, Parameters} ->
	    Converted = convert_command(Command, Parameters),
	    send_to_client(PortNr, Converted),
	    send_loop(PortNr);
	
	{quit, PortNr} ->
	    debug:info("FTPDriver for connection on port ~w quitting.", [PortNr]);

	Unknown ->
	    debug:info("unknown command received in ftp_driver: ~w", [Unknown]),
	    send_loop(PortNr)
    end.


% sends an FTPMessage to a given client on port PortNr.
send_to_client(_PortNr, _FTPMessage) ->
    true.


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
    debug:info("in convert_command/1, command given: ~p", [FTPCommandString]),
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
