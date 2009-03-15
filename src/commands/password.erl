-module(password).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, State, Password) ->
    case user:verify_login(State#state{password = Password}) of
	true ->
	    NewState = State#state { password = Password, status = logged_in },
	    FtpConnPid ! {reply, user_logged_in, [], NewState};
	false ->
	    NewState = State#state { status = invalid },
	    FtpConnPid ! {reply, not_logged_in, [], NewState}
    end.
