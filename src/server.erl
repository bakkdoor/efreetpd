-module(server).
%-export([start/1, loop/1]).
-compile(export_all).
-import(ftp_driver, [reply_code/1, reply_string/1, send_reply/2, send_reply/3]).
-import(tcp, [get_request/2, parse_request/1, parse_address/1, format_address/1, format_address/2, alpha/1]).
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


stop() ->
    server ! stop,
    unregister(server).

start(ListenPort) ->
    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {active, false}, {packet, 0}]),
    debug:info("server startet on port ~p", [ListenPort]),
    spawn(fun() -> accept_loop(LSocket) end),
    ServerPid = spawn(fun() ->
				   receive
				       stop ->
					   gen_tcp:close(LSocket)
				   end
		      end),
    register(server, ServerPid),
    ServerPid.
    
				   
accept_loop(LSocket) ->				   
    case gen_tcp:accept(LSocket) of
	{ok, NewSocket} -> 
	    debug:info("new connection on socket ~p (~p)", [NewSocket, inet:peername(NewSocket)]),
	    spawn(fun() -> accept_loop(LSocket) end),
						%loop(NewSocket).
	    acknowledge(NewSocket);
	{error, closed} ->
	    debug:info("accept_loop failed: listening socket closed."),
	    debug:info("stopping...");
	{error, Why} ->
	    debug:error("accept_loop failed: ~p", [Why]),
	    debug:error("stopping...")
    end.


% send acknowledge packet to client.
acknowledge(Socket) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {IP, Port}} = inet:peername(Socket),
    debug:info("sending acknowledge message to client (# ~p:~p)", [IP, Port]),
    send_reply(Socket, 220, Hostname ++ " eFreeTPd server v0.1 waiting."),
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
		    send_reply(Socket, 500, "syntax error: " ++ Request),
		    connection_loop(Socket, State, Buf1)
	    end;
	{error, closed} ->
	    debug:info("!! connection to ~p closed on port ~p:", [IP, Port]),
	    true
    end.

user_name(Name, Socket, St) ->
    Code = reply_code(user_name_ok),
    send_reply(Socket, Code),
    Homedir = config:setting(root_dir) ++ "/" ++ Name, 
    St#state { user = Name, 
	       home_dir = Homedir,
	       current_dir = Homedir }.

password(Password, Socket, St) ->
    verify_login(Socket, St#state{password = Password}),
    %% check that we have executed user and need a password
    %% then that the password is valid
    Code = reply_code(user_logged_in),
    send_reply(Socket, Code, "User " ++ St#state.user ++ " logged in, proceed"),
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
	    send_reply(Socket, reply_code(not_logged_in)), throw(failed)
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
		   send_reply(S, NotImplemented), throw(St)
	   end,
    send_reply(S,200,"new type " ++ atom_to_list(element(1,Type))),
    St#state { transfer_type = Type }.


print_work_dir(_Args, Socket, State = #state{current_dir = WorkDir}) ->
    assert_logged_in(Socket, State),
    send_reply(Socket, reply_code(pathname_created), "\"" ++ abs_name(WorkDir) ++ "\""),
    State.

passive_mode(_Args, Socket, State) ->
    assert_logged_in(Socket, State),
    St1 = close_listen(State),
    {ok,{Addr,_}} = inet:sockname(Socket),
    case gen_tcp:listen(0, [{active,false}, binary]) of
	{ok,ListenSock} ->
	    {ok,{_,Port}} = inet:sockname(ListenSock),
	    send_reply(Socket, reply_code(entering_passive_mode),"Entering Passive Mode (" ++
		  format_address(Addr,Port) ++ ")."),
	    St1#state { listen_socket = ListenSock };
	{error,Err} ->
	    send_reply(Socket, reply_code(cant_open_data_connection), erl_posix_msg:message(Err)),
	    St1
    end.

list_files(_Arg, Socket, _State = #state{current_dir = CurrentDir}) ->
    case file:list_dir(CurrentDir) of
	{ok, FileList} ->
	    lists:foreach(fun(File) ->
				  gen_tcp:send(Socket, File ++ tcp:crnl())
			  end,
			  FileList),
	    send_reply(Socket, reply_code(closing_data_connection));
	{error,Err} ->
	    send_reply(Socket, 550, "\"" ++ CurrentDir ++ "\" " ++
		  file:format_error(Err))
    end.
    

open_data_port(Args, Socket, State) ->
    assert_logged_in(Socket, State),
    St1 = close_listen(State),
    {ok,AddrPort} = parse_address(Args),
    send_reply(Socket,200),
    St1#state { data_port = AddrPort, listen_socket = undefined }.


%% command not implemented
cmd_not_implemented(_, Socket, State) ->
    send_reply(Socket, 502),    
    State.


%% check that a user has logged in and report errors
assert_logged_in(Socket, State) when is_record(State, state) ->
    case State#state.status of
	invalid -> send_reply(Socket, reply_code(not_logged_in)), throw(failed);
	waiting_for_pass ->  send_reply(Socket, reply_code(user_name_ok)), throw(failed);
	logged_in -> true
    end.



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


