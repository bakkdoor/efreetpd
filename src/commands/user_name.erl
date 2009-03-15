-module(user_name).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, State, Username) ->
    Homedir = user:home_dir(Username),
    NewState = State#state { user = Username, 
			     home_dir = Homedir,
			     current_dir = Homedir },
    FtpConnPid ! {reply, user_name_ok, [], NewState}.

