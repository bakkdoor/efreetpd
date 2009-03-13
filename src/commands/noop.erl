-module(noop).
-export([start/0]).

start(FTPConnPid, State) -> io:format("No Operation! ~n").
