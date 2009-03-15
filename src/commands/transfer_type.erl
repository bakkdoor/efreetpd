-module(transfer_type).
-export([start/3]).
-include("../state.hrl").
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

start(FtpConnPid, State, TypeArg) ->
    % user needs to be logged in
    case user:verify_login(State) of
	
	false ->
	    FtpConnPid ! {reply, not_logged_in, [], State},
	    exit(failed);

	true ->
	    Type = get_type(TypeArg),
	    case Type of 
		invalid ->
		    FtpConnPid ! {reply, command_not_implemented_for_param, [TypeArg], State},
		    exit(failed);
		
		_ ->
		    % if everything worked, set transfer_type to Type and reply to ftp_connection process
		    NewState = State#state { transfer_type = Type },
		    FtpConnPid ! {reply, command_ok, ["new type " ++ atom_to_list(element(1, Type))], NewState}
	    end
    end.


get_type(TypeArg) ->
    case tcp:alpha(hd(TypeArg)) of
	% 'i' -> image (binary)
	$i -> {image,nonprint,8};
	% 'a' -> ascii text
	$a -> {ascii,nonprint,8};
	% anything else isnt allowed:
	_ -> invalid
    end.
