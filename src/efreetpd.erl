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
    ListenPort = config:setting(Settings, main_listen_port),
    debug:info("starting eFreeTPd with StartPort ~p", [StartPort]),
    debug:info("all settings: ~p", [Settings]),
    MainPid = server:start(ListenPort, StartPort),
    
    % error-handler for main process
    utils:on_exit(MainPid, 
		  fun(_Pid, Why) ->
			  debug:error("main process stopped: ~p", [Why]),
			  debug:error("trying to restart main process...~n"),
			  efreetpd:start()
		  end).
