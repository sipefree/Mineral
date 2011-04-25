%%% @doc
%%% Records for messages (Client to Server packets)
%%% @end


-record(cli_keep_alive, {
      packet_id = 0
     }).

-record(cli_login_request, {
      packet_id = 1,
      protocol_version = 11,
      username,
      map_seed,
      dimension
     }).

-record(cli_handshake, {
      packet_id = 2,
      username
     }).

-record(cli_chat_message, {
      packet_id = 3,
      message
     }).

