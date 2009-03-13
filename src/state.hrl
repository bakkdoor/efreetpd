% ftp connection state record.
% holds data for each connection.

-record(state, 
	{
	  current_dir,
	  user,
	  password,
	  port,
	  ip,
	  data_port,
	  transfer_type,
	  transfer_mode
	 }).
	  
	  
