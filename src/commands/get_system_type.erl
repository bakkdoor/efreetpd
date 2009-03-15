-module(get_system_type).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, _State, _Arg) ->
    % not yet implemented
    FtpConnPid ! {reply, cmd_not_implemented, get_system_type}.
