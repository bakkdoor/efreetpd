-module(config).
-export([root_dir/0]).

%% specifies the root dir, in which all user directories are.
%% later on, this should be read from a config file...
root_dir() ->
    "/home/bakkdoor/projekte/erlang/eFreeTPd/root_dir".
