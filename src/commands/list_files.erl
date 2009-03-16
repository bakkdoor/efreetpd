-module(list_files).
-export([start/3]).
-include("../state.hrl").
-include_lib("kernel/include/file.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


start(FtpConnPid, State, Arg) ->
    user:assert_logged_in(FtpConnPid, State,
			  fun() ->
				  list_files(FtpConnPid, State, Arg)
			  end).

%% list_files(FtpConnPid, State = #state{current_dir = CurrentDir}, _Arg) ->
%%     case file:list_dir(CurrentDir) of
%% 	{ok, FileList} ->
%% 	    FtpConnPid ! {reply, {dir_listing, CurrentDir, FileList}};
	
%% 	{error,Err} ->
%% 	    FtpConnPid ! {reply, 
%% 			  550, 
%% 			  ["\"" ++ CurrentDir ++ "\" " ++ file:format_error(Err)],
%% 			  State}
%%     end.



%% TODO: need to open a data port/socket somehow
%% and send files over that port to client.
%% maybe somehow like this:

list_files(FtpConnPid, State, Arg) ->
    % we need to start a data_connection, so send a request
    FtpConnPid ! {request, data_connection, self(), State},
    {ok, DataConnPid, NewState} = wait_for_data_connection(),
    
    
    DirR = utils:rel_name(Arg, State#state.current_dir),
    Dir = filename:join(State#state.home_dir, DirR),


    dir_list(FtpConnPid, DataConnPid, NewState, Dir, DirR),
    
    % when done, close the data_connection process.
    DataConnPid ! {close, NewState},
    % and report to ftp_connection process.
    FtpConnPid ! {reply, command_ok, [], NewState}.


dir_list(FtpConnPid, DataConnPid, State, Dir1, Dir) ->
    case file:list_dir(Dir1) of
	{ok, List} ->
	    lists:foreach(
	      fun(E) ->
		      DataConnPid ! {data_reply, list_info(Dir1, E) ++ "\r\n"}
	      end, 
	      List),
	    
	    FtpConnPid ! {reply, closing_data_connection, [], State};

	{error,Err} ->
	    FtpConnPid ! {reply, 
			  550, 
			  ["\"" ++ Dir ++ "\" " ++ file:format_error(Err)],
			  State}
    end.


wait_for_data_connection() ->
    receive
	{request_reply, data_connection, DataConnPid, NewState} ->
	    {ok, DataConnPid, NewState};
	
	_Other ->
	    wait_for_data_connection()
	
	%after 5000 ->
%		{error, timeout}
	end.



list_info(Dir, File) ->
    case file:read_file_info(filename:join(Dir,File)) of
	{ok, Info} ->
	    finfo(Info) ++ " " ++ File;
	{error,_} ->
	    "???"
    end.


%%
%% format as access(10) + size(8)+ mdate(8)+ mtime(5)+ filename(n)
%%
finfo(Info) ->
    fmt_type(Info#file_info.type) ++ 
	fmt_access(Info#file_info.mode) ++ " " ++
	fmt_number(Info#file_info.size,8,$ ) ++ " " ++
	fmt_date(Info#file_info.mtime) ++ " " ++ 
	fmt_time(Info#file_info.mtime).


fmt_type(regular) -> "-";
fmt_type(directory) -> "d";
fmt_type(_) -> "?".

     
fmt_access(Mode) ->
    fmt_rwx(Mode) ++ fmt_rwx(Mode bsr 3) ++ fmt_rwx(Mode bsr 6).



fmt_rwx(Mode) ->
    [if Mode band 4 == 0 -> $-; true -> $r end,
     if Mode band 2 == 0 -> $-; true -> $w end,
     if Mode band 1 == 0 -> $-; true -> $x end].



fmt_date({{Y,M,D},_}) ->
    fmt_number(Y rem 100,2,$0) ++ "-" ++ 
	fmt_number(M,2,$0) ++ "-" ++
	fmt_number(D,2,$0).

fmt_time({_,{H,M,_S}}) ->
    fmt_number(H,2,$0) ++ ":" ++ 
	fmt_number(M,2,$0).

fmt_number(X, N, LeftPad) when X >= 0 ->
    Ls = integer_to_list(X),
    Len = length(Ls),
    if Len >= N -> Ls;
       true ->
	    lists:duplicate(N - Len, LeftPad) ++ Ls
    end.
		    
	    
