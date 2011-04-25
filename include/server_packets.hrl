%%% @doc
%%% Records for messages (Server to Client packets)
%%% @end


-record(login_response, {
      packet_id = 1,
      player_entity_id,
      unused_string,
      map_seed,
      dimension
     }).

-record(handshake, {
      packet_id = 2,
      connection_hash
     }).

-record(chat_message, {
      packet_id = 3,
      message
     }).

