-module(delFile).
-export([start/3]).

start(FtpConnPid, User, File) -> 
    case user:check_file(User, File) of
	true ->
	    file:delete(File),
	    FtpConnPid ! {reply, {file_deleted, File}};
	false ->
	    exit(FtpConnPid, {illegal_file, File})
    end.
