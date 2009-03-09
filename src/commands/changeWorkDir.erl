-module(changeWorkDir).
-export([start/3]).

start(FtpConnPid, User, NewDir) -> 
    case user:check_dir(User, NewDir) of
	true ->
	    DirContents = utils:get_dir_contents(NewDir),
	    FtpConnPid ! {reply, {dir_listing, NewDir, DirContents}};
	false ->
	    exit(FtpConnPid, {illegal_directory, NewDir})
    end.
