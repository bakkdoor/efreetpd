-module(server).
%-export([start/1, loop/1]).
-compile(export_all).
-include("state.hrl").
-author({"Denis Meyer", "calltopower88@web.de"}).

%% % Starts the Server from Port Port_Number
%% start(Max_Number_of_Ports) when integer(Max_Number_of_Ports), Max_Number_of_Ports > 9999, Max_Number_of_Ports < 65001 ->
%%     spawn(server, loop, [Max_Number_of_Ports]).

%% % Prooves user-name and password and connects to a new port if login successfull
%% loop(Next_free_Port) when integer(Next_free_Port), Next_free_Port > 9999, Next_free_Port < 65001 ->
%%     receive
%% 	{login, Pid_From, Name, Password} ->
%% 	    {Auth_status, _Pid, _Name} = authenticate:proove(Pid_From, Name, Password),
%% 	    case Auth_status of
%% 		true ->
%% 		    spawn(ftp_connection, start, [Next_free_Port]),
%% 		    loop(Next_free_Port + 1);
%% 		_Other ->
%% 		    io:format("Login not successful!"),
%% 		    loop(Next_free_Port)
%% 	    end
%%     end.

-define(CRNL, "\r\n").
-define(R_OKAY, 200).

start(ListenPort, _Next_free_Port) ->
    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {active, true}, {packet, 0}]),
    debug:info("server startet on port ~p", [ListenPort]),
    spawn(fun() -> accept_loop(LSocket) end).
    
				   
accept_loop(LSocket) ->				   
    {ok, NewSocket} = gen_tcp:accept(LSocket),
    debug:info("new connection on socket ~p (~p)", [NewSocket, inet:peername(NewSocket)]),
    spawn(fun() -> accept_loop(LSocket) end),
    %loop(NewSocket).
    acknowledge(NewSocket).
    

loop(Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    receive
	{tcp, Socket, Bin} ->
	    debug:info("!! received binary packet: ~p (# ~p:~p)", [binary_to_list(Bin), IP, Port]),
	    loop(Socket);
	{tcp_closed, Socket} ->
	    debug:info("!! connection closed on socket ~p (# ~p:~p)", [Socket, IP, Port])
    end.

%% ctl_loop_init(Ctl, Root, DefaultDataPort) ->
%%     {ok,Name} = inet:gethostname(),
%%     rsend(Ctl,220, Name ++ " Erlang Ftp server 1.0 ready."),
%%     ctl_loop(Ctl, #cstate { rootwd = Root,
%% 			    data_port = DefaultDataPort,
%% 			    def_data_port = DefaultDataPort }, []).

rsend(S, Code, Mesg) when is_integer(Code) ->
    gen_tcp:send(S, [integer_to_list(Code)," ",Mesg, ?CRNL]).



acknowledge(Socket) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {IP, Port}} = inet:peername(Socket),
    debug:info("sending acknowledge message to client (# ~p:~p)", [IP, Port]),
    rsend(Socket, 220, Hostname ++ " eFreeTPd server v0.1 waiting."),
    RootDir = config:setting(root_dir),
    connection_loop(Socket, 
		    #state{current_dir = RootDir, ip = IP, port = Port, data_port = Port - 1},
		   []). % empty buffer


connection_loop(Socket, State, Buf) ->
    debug:info("in connection loop..."),
    case get_request(Socket, Buf) of
	{ok, Request, Buf1} ->
	    case parse_request(Request) of
		{Fun, Args} ->
		    debug:info("got request: ~p with params ~p", [Fun, Args]);
		error ->
		    rsend(Socket, 500, "syntax error: " ++ Request),
		    connection_loop(Socket, State, Buf1)
	    end;
	{error, closed} ->
	    true
    end.

%% ctl_loop(Ctl, St, Buf) ->
%%     case ctl_line(Ctl,Buf) of
%% 	{ok,Line,Buf1} ->
%% 	    case ctl_parse(Line) of
%% 		{Fun,Args} ->
%% 		    case catch Fun(Args,Ctl,St) of
%% 			failed -> ctl_loop(Ctl,St,Buf1);
%% 			quit -> true;
%% 			init -> ctl_loop_init(Ctl, St#cstate.rootwd,
%% 					      St#cstate.def_data_port);
%% 			St1 when is_record(St1, cstate) ->
%% 			    ctl_loop(Ctl,St1,Buf1);
%% 			_ -> %% Crash etc
%% 			    rsend(Ctl,501,"argument error: " ++ Line),
%% 			    ctl_loop(Ctl,St,Buf1)
%% 		    end;
%% 		error ->
%% 		    rsend(Ctl,500,"syntax error: " ++ Line),
%% 		    ctl_loop(Ctl, St, Buf1)
%% 	    end;
%% 	{error,closed} ->
%% 	    true
%%     end.




%% parse a command and arguments
%% must be case insensitive on commands and type letters but
%% sensitive on path/user 
%% 
parse_request([L1,L2,L3 | T]) ->
    C1 = alpha(L1),
    C2 = alpha(L2),
    C3 = alpha(L3),
    case T of
	[] -> parse_request(list_to_atom([C1,C2,C3]), []);
	[$ | Arg] -> parse_request(list_to_atom([C1,C2,C3]),Arg);
	[C4] -> parse_request(list_to_atom([C1,C2,C3,alpha(C4)]),[]);
	[C4,$  | Arg] -> parse_request(list_to_atom([C1,C2,C3,alpha(C4)]),Arg);
	_ -> error
    end;
parse_request(_) -> error.

parse_request(Command, Args) ->
    {ftp_driver:convert_command(list_to_atom(Command)),
     Args}.

    
%% return lower letter space or ?		 
alpha(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alpha(X) when X >= $a, X =< $z -> X;
alpha(X) when X == $  -> X;
alpha(_) -> $?.


get_request(S, Buf) ->
    case split_line(Buf) of
	more ->
	    case gen_tcp:recv(S,0) of
		{ok,Cs} ->
		    Buf1 = Buf++Cs,
		    case split_line(Buf1) of
			more -> get_request(S, Buf1);
			Done -> Done
		    end;
		Error -> Error
	    end;
	Done -> Done
    end.

%% split a line after CRLF
split_line(Cs) ->
    split_line(Cs, []).

split_line([$\r,$\n|Cs], Buf) ->
    {ok, lists:reverse(Buf), Cs};
split_line([X|Cs], Buf) ->
    split_line(Cs, [X|Buf]);
split_line([], _) ->
    more.
