%% user-module.
%% some user-related functions.

-module(user).
-export([home_dir/1, check_dir/2, check_file/2, verify_login/1, assert_logged_in/3]).
-include("state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


%% returns the home directory of a given user.
-spec home_dir(string()) -> string().

home_dir(UserName) ->
    config:root_dir() ++ "/" ++ UserName.


%% checks, if a given folder/directory belongs to a given user.
%% returns true or false, indicating the ownership.
-spec check_dir(string(), string()) -> true | false.

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
-spec check_file(string(), string()) -> true | false | {error, string() | atom()}.

check_file(UserName, File) ->
    Dirname = filename:dirname(File),
    check_dir(UserName, Dirname).


verify_login(#state{user = User, password = Password}) ->
%%    EncryptedPassword = utils:encrypted_password_string(Password),
    Users = config:setting(users),
    case lists:member({User, Password}, Users) of
	true -> 
	    debug:info("user is correct: ~p - ~p", [User, Password]),
	    true;
	false -> 
	    debug:info("user is not correct: ~p - ~p", [User, Password]),
	    false
%	    ftp_driver:send_reply(Socket, not_logged_in), throw(failed)
   end.


%% when called, SucceedFun only gets called, if verify_login(State) returns true.
%% if not, FtpConnPid will be send a not_logged_in message, which results
%% in the client being disconnected.
assert_logged_in(FtpConnPid, State, SucceedFun) ->
    case user:verify_login(State) of
	
	false ->
	    FtpConnPid ! {reply, not_logged_in, [], State},
	    exit(failed);

	true ->
	    SucceedFun()
    end.
