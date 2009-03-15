-module(efreetpd).
-export([start/0, stop/0]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

%% 'main'-module.
%% contains the start function, which reads config-settings from config-file 
%% and starts the eFreeTPd server daemon.
start() ->
    crypto:start(),
    %% read config from configfile
    Settings = config:read("eFreeTPd.conf"),
    ListenPort = config:setting(Settings, main_listen_port),
    debug:info("starting eFreeTPd on port ~p", [ListenPort]),
    debug:info("all settings: ~p", [Settings]),
    MainPid = server:start(ListenPort),
    register(main_loop, MainPid). 
    
    % error-handler for main process
%%    utils:on_exit(MainPid, 
%% 		  fun(_Pid, Why) ->
%% 			  debug:error("main process stopped: ~p", [Why]),
%% 			  debug:error("trying to restart main process...~n"),
%% 			  efreetpd:start()
%% 		  end).

stop() ->
    main_loop ! stop.
%    server:stop().
