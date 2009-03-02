-module(ftp_connection).
-export([start/1]).

start(PortNr) ->
    %ftp_driver prozess starten, der auf PortNr hoert
    LoopPid = spawn(ftp_connection, receive_loop, []),
    FtpDriverPid = spawn(ftp_driver, start, [PortNr, LoopPid]).


receive_loop() ->
    % solange loopen, wie verbindung vorhanden
    receive
	{quit, PortNr} ->
	    io:format("verbindung beendet auf port: ~w~n", [PortNr]);

	{send_loop_pid, SendLoop) ->
	    spawn(ftp_connection, send_loop, [SendLoop]);

	{command, Command, Parameters} ->
	    execute_command(Command, Parameters),
	    loop();
	
	Unknown ->
	    io:format("unknown command: ~w~n", [Unknown]),
	    loop()
    end.


send_loop(DriverSendLoop) ->
    % schickt nachrichten an DriverSendLoop mit daten/befehlen/antworten,
    % die an den client geschickt werden sollen
    


% starts a new process for a given command.
execute_command(Command, Parameters) ->
    spawn(Command, start, Parameters).
