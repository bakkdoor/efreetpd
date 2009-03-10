-module(server).
-export([start/1, loop/1]).
-author({"Denis Meyer", "calltopower88@web.de"}).

% Starts the Server from Port Port_Number
start(Max_Number_of_Ports) when integer(Max_Number_of_Ports), Max_Number_of_Ports > 9999, Max_Number_of_Ports < 65001 ->
    spawn(server, loop, [Max_Number_of_Ports]).

% Prooves user-name and password and connects to a new port if login successfull
loop(Next_free_Port) when integer(Next_free_Port), Next_free_Port > 9999, Next_free_Port < 65001 ->
    receive
	{login, Pid_From, Name, Password} ->
	    {Auth_status, _Pid, _Name} = authenticate:proove(Pid_From, Name, Password),
	    case Auth_status of
		true ->
		    spawn(ftp_connection, start, [Next_free_Port]),
		    loop(Next_free_Port + 1);
		_Other ->
		    io:format("Login not successful!"),
		    loop(Next_free_Port)
	    end
    end.
