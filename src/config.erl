-module(config).
-export([root_dir/0, read_config/1, setting/2]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

%% specifies the root dir, in which all user directories are
root_dir() ->
    "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir".

% TODO
% Functions for reading setting(s) from a config file
read_config(_Filename) ->
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

setting(ConfigSettings, RequestedSetting) ->
    Filtered = lists:filter(fun ({Key, _Val}) -> Key =:= RequestedSetting end,
			    ConfigSettings),
    % return the first match
    lists:nth(1, Filtered). 
