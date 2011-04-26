%%% Author: Andrew Anderson <andrew.wja@gmail.com>
%%% @doc
%%% Records for messages (Server to Client packets)
%%% @end

-record(srv_keep_alive, {
    packet_id = 0
    }).

-record(srv_login_response, {
      packet_id = 1,
      player_entity_id,
      unused_string,
      map_seed,
      dimension
     }).

-record(srv_handshake, {
      packet_id = 2,
      connection_hash
     }).

-record(srv_chat_message, {
      packet_id = 3,
      message
     }).

-record(srv_inventory, {
      packet_id = 5,
      player_entity_id,
      slot_id,
      item_id,
      unknown
     }).
      
-record(srv_disconnect, {
    packet_id = 16#FF,
    reason
}).
