-module(server).
-export([start/1]).
-author({"Denis Meyer", "calltopower88@web.de"}).

% Starts the Server from Port Port_Number
start(Port_Number) ->
    spawn(server, loop, [Port_Number]).

% Prooves user-name and password and connects to a new port if login successfull
loop(Next_free_Port) ->
    receive
	{login, Pid_From, Name, Password} ->
	    {auth_status, pid, name} = authenticate:proove(Pid_From, Name, Password),
	    case auth_status of
		true ->
		    spawn(ftp_connection, start, [Next_free_Port]),
		    loop(Next_free_Port + 1);
		_Other ->
		    io:format("Login not successful!"),
		    loop(Next_free_Port)
	    end
    end.

% TODO
% Exception-Handling
% Later:
% Stop Server, Free Ports
