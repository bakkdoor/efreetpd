-module(noop).
-export([start/0]).

start() -> io:format("No Operation! ~n").
