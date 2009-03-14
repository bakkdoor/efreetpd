-module(server).
%-export([start/1, loop/1]).
-compile(export_all).
-import(ftp_driver, [reply_code/1, reply_string/1]).
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


%% sends a message (network-line-terminated status code and a message)
%% to the client (via Socket).
rsend(Socket, Code, Message) when is_integer(Code) ->
    gen_tcp:send(Socket, [integer_to_list(Code)," ",Message, ?CRNL]).

%% Send a sinlge line standard message
rsend(Socket, Code) ->
    gen_tcp:send(Socket, [reply_string(Code)++ ?CRNL]).


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
    Code = reply_code(user_name_ok),
    rsend(Socket, Code),
    Homedir = config:setting(root_dir) ++ "/" ++ Name, 
    St#state { user = Name, 
	       home_dir = Homedir,
	       current_dir = Homedir }.

password(Password, Socket, St) ->
    verify_login(Socket, St#state{password = Password}),
    %% check that we have executed user and need a password
    %% then that the password is valid
    Code = reply_code(user_logged_in),
    rsend(Socket, Code, "User " ++ St#state.user ++ " logged in, proceed"),
    St#state { password = Password, status = logged_in }.

verify_login(Socket, #state{user = User, password = Password}) ->
%%    EncryptedPassword = utils:encrypted_password_string(Password),
    Users = config:setting(users),
    case lists:member({User, Password}, Users) of
	true -> 
	    debug:info("user is correct: ~p - ~p", [User, Password]),
	    true;
	false -> 
	    debug:info("user is not correct: ~p - ~p", [User, Password]),
	    rsend(Socket, reply_code(not_logged_in)), throw(failed)
    end.

get_system_type(Args, Socket, State) ->
    cmd_not_implemented(Args, Socket, State).

transfer_type(Arg, S, St) ->
    assert_logged_in(S, St),
    Type = case alpha(hd(Arg)) of
	       % 'i' -> image (binary)
	       $i -> {image,nonprint,8};
	       % 'a' -> ascii text
	       $a -> {ascii,nonprint,8};
	       _ ->
		   NotImplemented = reply_code(command_not_implemented_for_param),
		   rsend(S, NotImplemented), throw(St)
	   end,
    rsend(S,200,"new type " ++ atom_to_list(element(1,Type))),
    St#state { transfer_type = Type }.


print_work_dir(_Args, Socket, State = #state{current_dir = WorkDir}) ->
    assert_logged_in(Socket, State),
    rsend(Socket, reply_code(pathname_created), "\"" ++ abs_name(WorkDir) ++ "\""),
    State.

passive_mode(_Args, Socket, State) ->
    assert_logged_in(Socket, State),
    St1 = close_listen(State),
    {ok,{Addr,_}} = inet:sockname(Socket),
    case gen_tcp:listen(0, [{active,false}, binary]) of
	{ok,ListenSock} ->
	    {ok,{_,Port}} = inet:sockname(ListenSock),
	    rsend(Socket, reply_code(entering_passive_mode),"Entering Passive Mode (" ++
		  format_address(Addr,Port) ++ ")."),
	    St1#state { listen_socket = ListenSock };
	{error,Err} ->
	    rsend(Socket, reply_code(cant_open_data_connection), erl_posix_msg:message(Err)),
	    St1
    end.

list_files(_Arg, Socket, _State = #state{current_dir = CurrentDir}) ->
    case file:list_dir(CurrentDir) of
	{ok, FileList} ->
	    lists:foreach(fun(File) ->
				  gen_tcp:send(Socket, File ++ ?CRNL)
			  end,
			  FileList),
	    rsend(Socket, reply_code(closing_data_connection));
	{error,Err} ->
	    rsend(Socket, 550, "\"" ++ CurrentDir ++ "\" " ++
		  file:format_error(Err))
    end.
    

open_data_port(Args, Socket, State) ->
    assert_logged_in(Socket, State),
    St1 = close_listen(State),
    {ok,AddrPort} = parse_address(Args),
    rsend(Socket,200),
    St1#state { data_port = AddrPort, listen_socket = undefined }.


%% command not implemented
cmd_not_implemented(_, Socket, State) ->
    rsend(Socket, 502),    
    State.

%% check that a user has logged in and report errors
assert_logged_in(Socket, State) when is_record(State, state) ->
    case State#state.status of
	invalid -> rsend(Socket, reply_code(not_logged_in)), throw(failed);
	waiting_for_pass ->  rsend(Socket, reply_code(user_name_ok)), throw(failed);
	logged_in -> true
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
    



close_listen(State) ->
    if State#state.listen_socket == undefined ->
	    State;
       true ->
	    gen_tcp:close(State#state.listen_socket),
	    State#state { listen_socket = undefined }
    end.

%%
%% We sometime need a simulated root, then call abs_name
%%
abs_name(Name) ->
    filename:join("/", Name).



parse_address(Str) ->
    paddr(Str, 0, []).


paddr([X|Xs],N,Acc) when X >= $0, X =< $9 -> paddr(Xs, N*10+(X-$0), Acc);
paddr([X|Xs],_N,Acc) when X >= $A, X =< $F -> paddr(Xs,(X-$A)+10, Acc);
paddr([X|Xs],_N,Acc) when X >= $a, X =< $f -> paddr(Xs, (X-$a)+10, Acc);
paddr([$,,$,|_Xs], _N, _Acc) -> error;
paddr([$,|Xs], N, Acc) -> paddr(Xs, 0, [N|Acc]);
paddr([],P2,[P1,D4,D3,D2,D1]) -> {ok,{{D1,D2,D3,D4}, P1*256+P2}};
paddr([],P2,[P1|As]) when length(As) == 32 ->
    case addr6(As,[]) of
	{ok,Addr} -> {ok, {Addr, P1*256+P2}};
	error -> error
    end;
paddr(_, _, _) -> error.

addr6([H4,H3,H2,H1|Addr],Acc) when H4<16,H3<16,H2<16,H1<16 ->
    addr6(Addr, [H4 + H3*16 + H2*256 + H1*4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.


format_address({A,B,C,D}, Port) ->
    integer_to_list(A) ++ "," ++
    integer_to_list(B) ++ "," ++
    integer_to_list(C) ++ "," ++
    integer_to_list(D) ++ "," ++
    integer_to_list(Port div 256) ++ "," ++
    integer_to_list(Port rem 256);
format_address({N1,N2,N3,N4,N5,N6,N7,N8},Port) ->
    h4(N1) ++ "," ++ h4(N2) ++ "," ++ h4(N3) ++ "," ++ h4(N4) ++ "," ++
    h4(N5) ++ "," ++ h4(N6) ++ "," ++ h4(N7) ++ "," ++ h4(N8) ++ "," ++
	integer_to_list(Port div 256) ++ "," ++
	integer_to_list(Port rem 256).

h4(N) ->
    [hx(N bsr 12),$,,hx(N bsr 8),$,,hx(N bsr 4),$,, hx(N)].

hx(N) ->
    N1 = N band 16#f,
    if N1 < 10 -> N1+$0;
       true -> (N1-10)+$A
    end.



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

