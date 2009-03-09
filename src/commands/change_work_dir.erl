-module(changeWorkDir).
-export([start/3]).

start(FtpConnPid, User, NewDir) -> 
    case user:check_dir(User, NewDir) of
	true ->
	    case file:list_dir(NewDir) of
		{ok, Listing} ->
		    FtpConnPid ! {reply, {dir_listing, NewDir, Listing}};
		{error, _Reason} ->
		    io:format("error while listing directory ~p: ~p~n", [NewDir, _Reason]),
		    exit(FtpConnPid, {illegal_directory, NewDir})
	    end;
	false ->
	    exit(FtpConnPid, {illegal_directory, NewDir})
    end.
