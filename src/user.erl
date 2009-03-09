-module(user).
-export([home_dir/1, check_dir/2]).

home_dir(UserName) ->
    config:root_dir() ++ "/" ++ UserName.


check_dir(UserName, Dir) ->
    UserHomeDir = home_dir(UserName),
    case string:str(Dir, UserHomeDir) of
	1 ->
	    filelib:is_dir(Dir);
	_Other ->
	    false
    end.


check_file(UserName, File) ->
    Dirname = filename:dirname(File),
    user:check_dir(Dirname).
    
