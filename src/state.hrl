% ftp connection state record.
% holds data for each connection.

-record(state, 
	{
	  current_dir,
	  user,
	  password,
	  port,
	  data_port,
	  transfer_type,
	  transfer_mode
	 }).
	  
	  
