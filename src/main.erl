-module(main).
-export([start/0]).
%-import(constants, [min_port]).

start() ->
    % socket auf port 21 erstellen
    % und auf neue connections warten
    % falls neue verbindung -> neuen unterprozess (ftp_connection/start) auf neuem port
    spawn(ftp_connection, start, [NeuerPort]),
    start().
