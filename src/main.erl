-module(main).
-export([start/0]).
%-import(constants, [min_port]).

start() ->
    % MinPort aus configdatei laden
    % und dann an start/1 uebergeben...
    MinPort = config:get_min_port(),
    start(MinPort).
    
start(MinPort) ->
    % socket auf port 21 erstellen
    % und auf neue connections warten
    % falls neue verbindung -> neuen unterprozess (ftp_connection/start) auf neuem port
    spawn(ftp_connection, start, [NeuerPort]),
    start(MinPort + 1). % naechster Port = MinPort + 1

