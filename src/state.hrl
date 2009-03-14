% ftp connection state record.
% holds data for each connection.

-record(state, 
	{
	  home_dir,
	  current_dir,
	  user,
	  password,
	  port,
	  ip,
	  data_port,
	  transfer_type,
	  transfer_mode
	 }).
	  
	  