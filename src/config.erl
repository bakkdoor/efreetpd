%% config-module.
%% functions related to the server-configuration.
%% for now, this is also the place to change the config-settings.

-module(config).
-export([root_dir/0, read/1, setting/1, setting/2]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


%% specifies the root dir, in which all user directories are
-spec root_dir() -> string().
    
root_dir() ->
    "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir".


-spec std_config_file() -> string().

std_config_file() ->
    "eFreeTPd.conf".



% TODO
% Functions for reading setting(s) from a config file
-spec read(string()) -> [{atom(), _Value}].

read(_Filename) ->
    %% read settings and return them as list of tuples (key-value pairs)
    %% for now, simply return the values statically...
    [
     {main_listen_port, 2222},
     % root directory, in which all user-directories lie.
     {root_dir, "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir"},
     % users list, first part is username (as string), second is a sha encrypted binary password,
     % represented as string of 8-bit hexadecimal integers via: utils:encrypted_password_string(PasswortString).
     {users, [{"test", "testpw"},
	      {"chris", "dasda"}]}
    ].

%% returns a specified setting-value (RequestedSetting)
%% from a list of settings (ConfigSettings)
-spec setting(list(), atom()) -> string() | non_neg_integer() | atom() | list().

setting(ConfigSettings, RequestedSetting) ->
    Filtered = lists:filter(fun ({Key, _Val}) -> Key =:= RequestedSetting end,
			    ConfigSettings),
    % return the first match, if there are any
    % otherwhise simply return undefined.
    case Filtered of
	[] ->
	    undefined;
	[_H|_T] ->
	    {_Key, Val} = lists:nth(1, Filtered),
	    Val % first match value.
    end.


%% works like setting/2, but reads the settings from a standard config-file
%% defined in std_config_file().
%% when first called, it saves the settings in the current processe's dictionary (via put)
%% in any subsequent calls to this function from the same process, it simply gets the settings
%% from the processe's dictionary (via get). the name of the key within the dictionary is: config_settings
-spec setting(atom()) -> string() | non_neg_integer() | atom().

setting(RequestedSetting) ->
    case get(config_settings) of
	undefined ->
	    put(config_settings, read(std_config_file()));
	_Else ->
	    nothing
    end,
    setting(get(config_settings), RequestedSetting).
