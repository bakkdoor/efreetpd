%% data_connection-module.
%% represents a data_connection process.

-module(data_connection).
-export([start/2]).
-include("state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(DataSocket, State) ->
    loop(DataSocket, State).


loop(DataSocket, State) ->
    receive
	{data_reply, Message} ->
	    gen_tcp:send(DataSocket, Message),
	    loop(DataSocket, State);

	{close, _NewState} ->
	    gen_tcp:close(DataSocket),
	    data_connection_done;
	
	_Other ->
	    loop(DataSocket, State)
    end.
