-module(user).
-export([home_dir/1, check_dir/2, check_file/2]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


%% returns the home directory of a given user.
home_dir(UserName) ->
    config:root_dir() ++ "/" ++ UserName.


%% checks, if a given folder/directory belongs to a given user.
%% returns true or false, indicating the ownership.
check_dir(UserName, Dir) ->
    UserHomeDir = home_dir(UserName),
    case string:str(Dir, UserHomeDir) of
	1 ->
	    filelib:is_dir(Dir);
	_Other ->
	    false
    end.


%% checks, if a given file belongs to a given user.
%% returns true or false, indicating the ownership.
check_file(UserName, File) ->
    Dirname = filename:dirname(File),
    check_dir(Dirname).
