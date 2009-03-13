-module(del_file).
-export([start/3]).
-include("../state.hrl").

start(FtpConnPid, _State = #state{current_dir = _CurrentDir, user= User}, File) -> 
    case user:check_file(User, File) of
	true ->
	    file:delete(File),
	    FtpConnPid ! {reply, {file_deleted, File}};
	false ->
	    exit(FtpConnPid, {illegal_file, File})
    end.
