-module(print_work_dir).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, State, _Arg) ->
    % if logged in send reply message.
    user:assert_logged_in(FtpConnPid, State,
			  fun() ->
				  send_reply(FtpConnPid, State)
			  end).

send_reply(FtpConnPid, State) ->
    WorkDir = State#state.current_dir,
    FtpConnPid ! {reply, pathname_created, ["\"" ++ abs_name(WorkDir) ++ "\""], State}.
				  

%%
%% We sometime need a simulated root, then call abs_name
%%
abs_name(Name) ->
    filename:join("/", Name).
