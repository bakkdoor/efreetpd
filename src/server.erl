-module(server).
-export([start/1]).
-import(authenticate, [start/1, proove/2]).
-author({"Denis Meyer", "calltopower88@web.de"}).

% Starts the Server from Port Port_Number
start(Port_Number) ->
    spawn(server, loop, [Port_Number]).

% Stops the Server: not needed by now = Ports lost
stop() ->
    % TODO
    ok.

% Waits for Request from Client
loop(Next_free_Port) ->
    receive
	{login, Pid_From, Name, Password} ->
	    {auth_status, pid, name} = authenticate:proove(Pid_From, Name, Password),
	    case auth_status of
		true ->
		    % TODO
		    % Portzuweisung + Prozessstart am zugewiesenen Port
		    loop(Next_free_Port + 1);
		Error ->
		    io:format("Not connected! Reason: ~w", Error),
		    loop(Next_free_Port)
	    end
    end.
