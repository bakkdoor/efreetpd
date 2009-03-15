-author({"Christopher Bertels", "bakkdoor@flasht.de"}).

% ftp connection state record.
% holds data for each connection and gets passed around
% to keep track of the connection state of each connected client.
-record(state, 
	{
	  % users home dir
	  home_dir,
	  % current work directory
	  current_dir,
	  % username
	  user,
	  % password
	  password,
	  % one of: invalid, waiting_for_pass, logged_in
	  status,
	  % port to client
	  port,
	  % client's ip adress
	  ip,
	  % data connection port to client
	  data_port,
	  % transfer type (ascii or binary)
	  transfer_type,
	  % transfer mode (active or passive)
	  transfer_mode,
	  % listening socket for data transfer
	  listen_socket = undefined
	 }).
	  
	  
