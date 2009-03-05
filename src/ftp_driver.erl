-module(ftp_driver).
-export([start/2]).

start(PortNr, FtpConnectionPid) ->
    % socket oeffnen auf port PortNr
    % und loopen
    ReceiveLoop = spawn(ftp_driver, receive_loop, [PortNr, FtpConnectionPid]),
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
	    io:format("unknown command received in ftp_driver: ~w", [Unkown]),
	    send_loop(PortNr)
    end.


% sends an FTPMessage to a given client on port PortNr.
send_to_client(PortNr, FTPMessage) ->
    true.


convert_command(Command, Parameters) ->
    true.
