%% Utilities module for eFreeTPd.
%% Some helper functions etc.

-module(utils).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).
-export([process_name/1, process_name/2, rpc/2, rpc/3, on_exit/2, keep_alive/2, encrypted_password_string/1]).
-export([write_file/2, read_file/1]).


%% returns an atom for use as a process name
%% example: 
%% process_name(10) 
%% -> connection_10
-spec process_name(integer()) -> atom().

process_name(Number) ->
    process_name("connection_", Number).


%% returns an atom for use as a process name with a given prefix
%% example: 
%% process_name("my_prefix", 10) 
%% -> my_prefix_10
-spec process_name(string() | list(), integer()) -> atom().

process_name(Prefix, Number) when is_list(Prefix) and is_integer(Number) ->
    NumberStr = erlang:integer_to_list(Number),
    erlang:list_to_atom(Prefix ++ NumberStr).


%% starts a remote procedure call (rpc) to a given process (Pid) with a given request.
%% returns the remote response, when it returns.
-spec rpc(pid(), atom() | tuple() | list()) -> atom() | tuple() | list() | _Value.

rpc(Pid, Request) ->
    Pid ! {rpc_request, {self(), Request}},
    receive
	{rpc_response, {Pid, Response}} ->
	    Response
    end.


%% starts a remote procedure call (rpc) to a given process (Pid) with a given timeout value.
%% if the request takes longer than the specified value, it simply returns timeout.
-spec rpc(pid(), atom() | tuple() | list(), integer()) -> atom() | tuple() | list() | _Value | timeout.

rpc(Pid, Request, Timeout) ->
    Pid ! {rpc_request, {self(), Request}},
    receive
	{rpc_response, {Pid, Response}} ->
	    Response
    after Timeout ->
	    timeout
    end.
			

%% starts a system process that executes a function with a given exit reason (Fun(Pid,Reason))
%% when a given process (Pid) dies.
-spec on_exit(pid(), fun()) -> pid().

on_exit(Pid, Fun) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		      {'EXIT', Pid, Why} ->
			  Fun(Pid, Why)
		  end
	  end).


-spec keep_alive(atom(), fun()) -> pid().

keep_alive(Name, Fun) ->
    Pid = spawn(Fun),
    register(Name, Pid),
    on_exit(Pid, 
	    fun(_Why) -> keep_alive(Name, Fun) end).


-spec encrypted_password_string(string()) -> string().

%% this is currently not working somehow..
encrypted_password_string(PasswordString) ->
    Started = get(crypto_module_started),
    case Started of
	true ->
	    nothing;
	undefined ->
	    crypto:start(),
	    put(crypto_module_started, true)
    end,
    CryptedPasswd = erlang:binary_to_list(crypto:sha(PasswordString)),
   lists:flatten(lists:map(fun(X) -> io_lib:format("~.16B", [X]) end, CryptedPasswd)).


%% writes a binary to a given filename.
-spec write_file(string(), binary()) -> ok | {error, string() | atom() | _Value}.

write_file(Filename, Bin) ->
    {ok, File} = file:open(Filename, [write, binary]),
    file:write(File, Bin),
    file:close(File).


%% reads the binary from a given filename.
-spec read_file(string()) -> binary() | {error, string() | atom() | _Value}.

read_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Binary.
