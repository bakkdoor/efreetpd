-module(ftp_driver).
-export([start/2, get_driver_pid/1]).

start(PortNr, FtpConnectionPid) ->
    % socket oeffnen auf port PortNr
    % und loopen
    _ReceiveLoop = spawn(ftp_driver, receive_loop, [PortNr, FtpConnectionPid]),
    SendLoop = spawn(ftp_driver, send_loop, [PortNr]),
    FtpConnectionPid ! {send_loop_pid, SendLoop}.

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
	    io:format("FTPDriver for connection on port ~w quitting.", [PortNr]);

	Unknown ->
	    io:format("unknown command received in ftp_driver: ~w", [Unknown]),
	    send_loop(PortNr)
    end.


% sends an FTPMessage to a given client on port PortNr.
send_to_client(PortNr, FTPMessage) ->
    true.


convert_command(Command, Parameters) ->
    true.



%to_ftp_packet({reply, {
to_ftp_packet(UnknownMessage) ->
    io:format("error: don't know how to convert this message to ftp-packet: ~p~n", [UnknownMessage]).


get_driver_pid(PortNr) ->
    ProcName = utils:process_name("ftp_driver_", PortNr),
    whereis(ProcName).
