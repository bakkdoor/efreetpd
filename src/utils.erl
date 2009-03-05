%% Utilities module for eFreeTPd.
%% Some helper functions etc.
-module(utils).
-export([process_name/1, process_name/2, rpc/2, rpc/3, on_exit/2]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

%% returns an atom for use as a process name
%% example: 
%% process_name(10) 
%% -> connection_10
process_name(Number) ->
    process_name("connection_", Number).

%% returns an atom for use as a process name with a given prefix
%% example: 
%% process_name("my_prefix", 10) 
%% -> my_prefix_10
process_name(Prefix, Number) when is_list(Prefix) and is_integer(Number) ->
    NumberStr = erlang:integer_to_list(Number),
    erlang:list_to_atom(Prefix, NumberStr).


%% starts a remote procedure call (rpc) to a given process (Pid) with a given request.
%% returns the remote response, when it returns.
rpc(Pid, Request) ->
    Pid ! {rpc_request, {self(), Request}},
    receive
	{rpc_response, {Pid, Response}} ->
	    Response
    end.


%% starts a remote procedure call (rpc) to a given process (Pid) with a given timeout value.
%% if the request takes longer than the specified value, it simply returns timeout.
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
on_exit(Pid, Fun) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		      {'EXIT', Pid, Why} ->
			  Fun(Pid, Why)
		  end
	  end).
