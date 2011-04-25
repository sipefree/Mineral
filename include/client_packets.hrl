%%% @doc
%%% Records for messages (Client to Server packets)
%%% @end


-record(keep_alive, {
	  packet_id = 0
	 }).

-record(login_request, {
	  packet_id = 1,
	  protocol_version = 11,
	  username
	 }).

-record(handshake, {
	  packet_id = 2,
	  username
	 }).

-record(chat_message, {
	  packet_id = 3,
	  message
	 }).

