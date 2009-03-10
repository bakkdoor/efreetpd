-module(debug).
-export([info/1, info/2, error/1, error/2]).

info(Message) ->
    io:format(">> " ++ Message ++ "~n").

info(FormatString, Args) ->
    io:format(">> " ++ FormatString ++ "~n", Args).

error(ErrorMessage) ->
    io:format(">>> error: " ++ ErrorMessage ++ "~n").

error(FormatString, Args) ->
    io:format(">>> error: " ++ FormatString ++ "~n", [Args]).

