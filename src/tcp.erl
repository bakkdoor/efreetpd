%% module for some tcp/ip related helper functions
-module(tcp).
-export([receive_binary/1]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


-spec receive_binary(port()) -> {ok, binary()} | {error, string() | atom()}.

receive_binary(Socket) ->
    receive_binary(Socket, []).


-spec receive_binary(port(), list()) -> {ok, binary()} | {error, string() | atom()}.

receive_binary(Socket, BinAcc) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    receive_binary(Socket, [BinAcc, B]);
	{error, closed} ->
	    {ok, list_to_binary(BinAcc)}
    end.
