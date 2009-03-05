%% Utilities module for eFreeTPd.
%% Some helper functions etc.
-module(utils).
-export([process_name/1, process_name/2]).
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
