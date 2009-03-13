-module(change_dir_up).
-export([start/2, get_parent_dir/2]).
-include("../state.hrl").
%ueberprüfen: userverzeichnis oder nicht (zusaetzliche Parameter!)

start(FtpConnPid, _State = #state{current_dir = CurrentDir, user = User}) -> 
    % vom aktuellen verzeichnis das höhere nehmen
    % und liste der dateien zurücksenden
    ParentDir = get_parent_dir(User, CurrentDir),
    case user:check_dir(User, ParentDir) of
	true ->
	    case file:list_dir(ParentDir) of
		{ok, Listing} ->
		    FtpConnPid ! {reply, {dir_listing, ParentDir, Listing}};
		{error, _Reason} ->
		    io:format("error while listing directory ~p: ~p~n", [ParentDir, _Reason]),
		    exit(FtpConnPid, {illegal_directory, ParentDir})
	    end;
	false ->
	    exit(FtpConnPid, {illegal_directory, ParentDir})
    end.    


get_parent_dir(User, Dir) ->
    HomeDir = user:home_dir(User),
    case Dir of
	HomeDir ->
	    HomeDir;
	_Other ->
	    case file:set_cwd(Dir) of
		ok ->
		    file:set_cwd(".."),
		    case file:get_cwd() of
			{ok, Dirname} ->
			    Dirname;
			{error, _Reason} -> 
			    Dir
		    end;
		{error, _Reason} ->
		    HomeDir
	    end
    end.
