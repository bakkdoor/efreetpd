-module(config).
-export([root_dir/0]).

%% specifies the root dir, in which all user directories are
root_dir() ->
    "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir".

% TODO
% Read setting(s) in from a config file