-module(changeDirUp).
-export([start/3, get_parent_dir/2]).
%ueberprüfen: userverzeichnis oder nicht (zusaetzliche Parameter!)

start(FtpConnPid, User, CurrentDir) -> 
    % vom aktuellen verzeichnis das höhere nehmen
    % und liste der dateien zurücksenden
    ParentDir = get_parent_dir(User, CurrentDir),
    case user:check_dir(User, ParentDir) of
	true ->
	    DirContents = utils:get_dir_contents(ParentDir),
	    FtpConnPid ! {reply, {dir_listing, ParentDir, DirContents}};
	false ->
	    exit(FtpConnPid, {illegal_directory, ParentDir})
    end.    


get_parent_dir(User, Dir) ->
    HomeDir = user:home_dir(User),
    case Dir of
	HomeDir ->
	    HomeDir;
	_ ->
	    case file:set_cwd(Dir) of
		ok ->
		    file:set_cwd(".."),
		    case file:get_cwd() of
			{ok, Dirname} ->
			    Dirname;
			{error, _Reason} -> 
			    Dir
		    end;
		{error, _} ->
		    HomeDir
	    end
    end.
