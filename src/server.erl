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

% network line terminator (terminates/seperates a ftp message from another).
-define(CRNL, "\r\n").
% okay status flag.
-define(R_OKAY, 200).

stop() ->
    server ! stop.

start(ListenPort, _Next_free_Port) ->
    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {active, false}, {packet, 0}]),
    debug:info("server startet on port ~p", [ListenPort]),
    spawn(fun() -> accept_loop(LSocket) end),
    register(server, spawn(fun() ->
				   receive
				       stop ->
					   gen_tcp:close(LSocket)
				   end
			   end)).
    
				   
accept_loop(LSocket) ->				   
    {ok, NewSocket} = gen_tcp:accept(LSocket),
    debug:info("new connection on socket ~p (~p)", [NewSocket, inet:peername(NewSocket)]),
    spawn(fun() -> accept_loop(LSocket) end),
    %loop(NewSocket).
    acknowledge(NewSocket).
    

%% ctl_loop_init(Ctl, Root, DefaultDataPort) ->
%%     {ok,Name} = inet:gethostname(),
%%     rsend(Ctl,220, Name ++ " Erlang Ftp server 1.0 ready."),
%%     ctl_loop(Ctl, #cstate { rootwd = Root,
%% 			    data_port = DefaultDataPort,
%% 			    def_data_port = DefaultDataPort }, []).


%% sends a message (network-line-terminated status code and a message)
%% to the client (via Socket).
rsend(Socket, Code, Message) when is_integer(Code) ->
    gen_tcp:send(Socket, [integer_to_list(Code)," ",Message, ?CRNL]).

%% Send a sinlge line standard message
rsend(Socket, Code) ->
    gen_tcp:send(Socket, [ftp_driver:reply_string(Code)++ ?CRNL]).


% send acknowledge packet to client.
acknowledge(Socket) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {IP, Port}} = inet:peername(Socket),
    debug:info("sending acknowledge message to client (# ~p:~p)", [IP, Port]),
    rsend(Socket, 220, Hostname ++ " eFreeTPd server v0.1 waiting."),
    RootDir = config:setting(root_dir),
    connection_loop(Socket, 
		    #state{current_dir = RootDir, ip = IP, port = Port, data_port = Port - 1},
		   []). % empty buffer


connection_loop(Socket, State = #state{ip = IP, port = Port}, Buf) ->
    debug:info("in connection loop..."),
    debug:info("current state: ~p", [State]),
    case get_request(Socket, Buf) of
	{ok, Request, Buf1} ->
	    case parse_request(Request) of
		{Command, Args} ->
		    debug:info("got request: ~p with params ~p", [Command, Args]),
		    % call handler-function for request:
		    %spawn(?MODULE, Command, [Args, Socket, State]),
		    case catch apply(?MODULE, Command, [Args, Socket, State]) of
			failed -> connection_loop(Socket, State, Buf1);
			quit -> true;
			St1 when is_record(St1, state) ->
			    connection_loop(Socket, St1, Buf1)
		    end;
		    %connection_loop(Socket, State, Buf1);
		error ->
		    rsend(Socket, 500, "syntax error: " ++ Request),
		    connection_loop(Socket, State, Buf1)
	    end;
	{error, closed} ->
	    debug:info("!! connection to ~p closed on port ~p:", [IP, Port]),
	    true
    end.

user_name(Name, Socket, St) ->
    Code = ftp_driver:reply_code(user_name_ok),
    rsend(Socket, Code),
    Homedir = config:setting(root_dir) ++ "/" ++ Name, 
    St#state { user = Name, 
	       home_dir = Homedir,
	       current_dir = Homedir }.

password(Password, Socket, St) ->
    verify_login(Socket, St#state{password = Password}),
    %% check that we have executed user and need a password
    %% then that the password is valid
    Code = ftp_driver:reply_code(user_logged_in),
    rsend(Socket, Code, "User " ++ St#state.user ++ " logged in, proceed"),
    St#state { password = Password }.

verify_login(Socket, #state{user = User, password = Password}) ->
%%    EncryptedPassword = utils:encrypted_password_string(Password),
    Users = config:setting(users),
    case lists:member({User, Password}, Users) of
	true -> 
	    debug:info("user is correct: ~p - ~p", [User, Password]),
	    true;
	false -> 
	    debug:info("user is not correct: ~p - ~p", [User, Password]),
	    rsend(Socket, ftp_driver:reply_code(not_logged_in)), throw(failed)
    end.

get_system_type(Args, Socket, State) ->
    cmd_not_implemented(Args, Socket, State).

%% command not implemented
cmd_not_implemented(_, Socket, State) ->
    rsend(Socket, 502),    
    State.


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
	[] -> parse_request([C1,C2,C3], []);
	[$ | Arg] -> parse_request([C1,C2,C3],Arg);
	[C4] -> parse_request([C1,C2,C3,alpha(C4)],[]);
	[C4,$  | Arg] -> parse_request([C1,C2,C3,alpha(C4)],Arg);
	_ -> error
    end;
parse_request(_) -> error.

parse_request(Command, Args) ->
    {ftp_driver:convert_command(Command),
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
		{ok, Cs} ->
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
split_line(Bin) when is_binary(Bin) ->
    split_line(binary_to_list(Bin));
split_line(Cs) ->
    split_line(Cs, []).

split_line([$\r,$\n|Cs], Buf) ->
    {ok, lists:reverse(Buf), Cs};
split_line([X|Cs], Buf) ->
    split_line(Cs, [X|Buf]);
split_line([], _) ->
    more.

