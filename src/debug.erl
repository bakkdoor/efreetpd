%% debug-module.
%% holds some debugging related helperfunctions,
%% mainly for i/o of info/error messages.

-module(debug).
-export([info/1, info/2, error/1, error/2]).

-spec info(string()) -> ok | {error, string()}.
info(Message) ->
    io:format(">> " ++ Message ++ "~n").


-spec info(string(), list()) -> ok | {error, string()}.

info(FormatString, Args) ->
    io:format(">> " ++ FormatString ++ "~n", Args).


-spec error(string()) -> ok | {error, string()}.   

error(ErrorMessage) ->
    io:format(">>> error: " ++ ErrorMessage ++ "~n").


-spec error(string(), list()) -> ok | {error, string()}.   

error(FormatString, Args) ->
    io:format(">>> error: " ++ FormatString ++ "~n", [Args]).

