-module(list_files).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, State, _Arg) ->
    user:assert_logged_in(FtpConnPid, State,
			  fun() ->
				  list_files(FtpConnPid, State)
			  end).


list_files(FtpConnPid, State = #state{current_dir = CurrentDir}) ->
    case file:list_dir(CurrentDir) of
	{ok, FileList} ->
	    FtpConnPid ! {reply, {dir_listing, CurrentDir, FileList}};

	{error,Err} ->
	    FtpConnPid ! {reply, 
			  550, 
			  ["\"" ++ CurrentDir ++ "\" " ++ file:format_error(Err)],
			  State}
    end.
