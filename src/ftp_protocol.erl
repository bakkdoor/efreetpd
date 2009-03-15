%% ftp_protocol-module.
%% holds functions for converting messages back and forth
%% from our internal message-types to actual ftp-protocol commands.
%% also defines a set of standard reply messages to the client.

-module(ftp_protocol).
-export([ftp_command_mappings/0, reply_code/1, reply_string/1]).
-author({"Christopher Bertels", "bakkdoor@flasht.de"}).


%% returns a list of tuples containing the ftp protocoll command name (as an atom)
%% and the internally used name (as an atom) of each ftp command.
ftp_command_mappings() ->
    [{abor, abort_transfer},
     {cwd, change_work_dir},
     {dele, del_file},
     {list, list_files},
     {mdtm, get_modification_time},
     {nlst, name_list_remote_dir},
     {pass, password},
     {pasv, passive_mode},
     {port, open_data_port},
     {pwd, print_work_dir},
     {quit, quit_connection},
     {retr, retrieve_file},
     {rmd, remove_dir},
     {rnfr, rename_from},
     {rnto, rename_to},
     {site, site},
     {size, get_file_size},
     {stor, store_file},
     {type, transfer_type},
     {user, user_name},
     % from here on less common commands:
     % (as of: http://www.nsftools.com/tips/RawFTP.htm)
     {acct, account_info},
     {appe, append_to_file},
     {cdup, change_dir_up},
     {help, get_help},
     {mode, transfer_mode},
     {noop, noop},
     {rein, reinitialize_connection},
     {stat, get_server_status},
     {stou, store_file_uniquely},
     {stru, file_transfer_structure},
     {syst, get_system_type}].


%% this is simply for easier usage, so if we get the actual code,
%% simply return it.
reply_code(Code) when is_integer(Code) ->
    Code;
reply_code(ReplyName) when is_atom(ReplyName) ->
    case ReplyName of
	mark_yyyy_mmmm -> 110;
	service_ready_in -> 120;
	data_connect_already_open -> 125;
	file_status_ok -> 150;
	command_ok -> 200;
	command_not_implemented -> 202;
	system_status -> 211;
	dir_status -> 212;
	file_status -> 213;
	help_message -> 214;
	name_system_type -> 215;
	service_ready -> 220;
	service_closing_control_connection -> 221;
	data_connection_open -> 225;
	closing_data_connection -> 226;
	entering_passive_mode -> 227;
	user_logged_in -> 230;
	req_file_action_ok -> 250;
	pathname_created -> 257;
	user_name_ok -> 331;
	need_account_for_login -> 332;
	req_file_action_pending -> 350;
	service_not_available -> 421;
	cant_open_data_connection -> 425;
	connection_closed -> 426;
	req_file_action_not_taken -> 450;
	req_action_error -> 451;
	req_action_not_taken -> 452;
	syntax_error -> 500;
	syntax_error_arguments -> 501;
	bad_seq_of_commands -> 503;
	command_not_implemented_for_param -> 504;
	not_logged_in -> 530;
	need_account_for_storing -> 532;
	req_action_aborted -> 551;
	req_file_action_aborted -> 552
    end.



%% Standard reply strings and theier meaning
reply_string(110) -> "110 MARK yyyy = mmmm";             %% ARGS
reply_string(120) -> "120 Service ready in nnn minutes.";  %% ARG
reply_string(125) -> "125 Data connection already open; transfere starting.";
reply_string(150) -> "150 File status okay; about to open data connection.";
reply_string(200) -> "200 Command okay.";
reply_string(202) -> "202 Command not implemented, superfluos at this site.";
reply_string(211) -> "211 System status, or system help reply.";
reply_string(212) -> "212 Directory status.";
reply_string(213) -> "213 File status.";
reply_string(214) -> "214 Help message.";     %% ADD HELP
reply_string(215) -> "215 NAME system type";  %% set NAME
reply_string(220) -> "220 Service ready for user.";
reply_string(221) -> "221 Service closing control connection.";
reply_string(225) -> "225 Data connection open; no transfere in progress";    
reply_string(226) -> "226 Closing data connection.";  %% ADD INFO
reply_string(227) -> "227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).";  %% ARGS
reply_string(230) -> "230 User logged in, proceed.";
reply_string(250) -> "250 Requested file action okay, completed.";
reply_string(257) -> "257 PATHNAME created.";  %% ARG
reply_string(331) -> "331 User name okay, need password.";
reply_string(332) -> "332 Need account for login.";
reply_string(350) -> "350 Requested file action pending further information.";
reply_string(421) -> "421 Service not available, closing control connection.";
reply_string(425) -> "425 Can't open data connection.";
reply_string(426) -> "426 Connection closed; transfere aborted.";
reply_string(450) -> "450 Requested file action not taken.";
reply_string(451) -> "451 Requested action not taken: local error in processing.";
reply_string(452) -> "452 Requested action not taken.";
reply_string(500) -> "500 Syntax error, command unrecognized.";  %% ADD INFO
reply_string(501) -> "501 Syntax error in paramters or arguments.";
reply_string(502) -> "502 Command not implemented.";
reply_string(503) -> "503 Bad sequence of commands.";
reply_string(504) -> "504 Command not implemented for that parameter.";
reply_string(530) -> "530 Not logged in.";
reply_string(532) -> "532 Need account for storing files.";
reply_string(550) -> "550 Requested action not taken.";
reply_string(551) -> "551 Requested action aborted: page type unkown.";
reply_string(552) -> "552 Requested file action aborted.";
reply_string(553) -> "553 Requested action not taken.";
reply_string(_) -> not_available.
