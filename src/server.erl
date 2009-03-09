-module(server).
-export([start/0]).
-import(authenticate, [start/1, proove/2]).
-author({"Denis Meyer", "calltopower88@web.de"}).

% Starts the Server
start() ->
    spawn(main_loop, loop, [10000]).

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
		_Other ->
		    loop(Next_free_Port)
	    end
    end.
