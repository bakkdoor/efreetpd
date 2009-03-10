-module(efreetpd).
-export([start/0]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

%% 'main'-module.
%% contains the start function, which reads config-settings from config-file 
%% and starts the eFreeTPd server daemon.
start() ->
    %% read config from configfile
    Settings = config:read("eFreeTPd.conf"),
    StartPort = config:setting(Settings, start_port),
    io:format(">> starting eFreeTPd with StartPort ~p~n", [StartPort]),
    io:format(">> all settings: ~p", [Settings]),
    MainPid = server:start(StartPort),
    
    % error-handler for main process
    utils:on_exit(MainPid, 
		  fun(Why) ->
			  io:format(">>> main process stopped: ~p~n", [Why]),
			  io:format(">>> trying to restart main process...~n"),
			  efreetpd:start()
		  end).
