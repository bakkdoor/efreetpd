-module(config).
-export([root_dir/0, read/1, setting/1, setting/2]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

%% specifies the root dir, in which all user directories are
root_dir() ->
    "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir".

std_config_file() ->
    "eFreeTPd.conf".

% TODO
% Functions for reading setting(s) from a config file
read(_Filename) ->
    %% read settings and return them as list of tuples (key-value pairs)
    %% for now, simply return the values statically...
    [
     % root directory, in which all user-directories lie.
     {root_dir, "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir"},
     % users list, first part is username (as string), second is a sha encrypted binary password,
     % represented as list of 8-bit integers via: erlang:binary_to_list(crypto:sha(PasswortString)).
     {users, [{"user1", [28,255,250,42,225,101,40,227,97,21,236,232,177,242,96,27,207,116,65,78]},
	      {"user2", [42,166,10,143,247,252,212,115,211,33,224,20,106,253,158,38,223,57,81,71]}]},
     {start_port, 10000}
    ].

%% returns a specified setting-value (RequestedSetting)
%% from a list of settings (ConfigSettings)
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
setting(RequestedSetting) ->
    case get(config_settings) of
	undefined ->
	    put(config_settings, read(std_config_file()));
	_Else ->
	    nothing
    end,
    setting(get(config_settings), RequestedSetting).
