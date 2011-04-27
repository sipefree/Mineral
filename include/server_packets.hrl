%%% Author: Andrew Anderson <andrew.wja@gmail.com>
%%% @doc
%%% Records for messages (Server to Client packets)
%%% @end

-define(PROTO_KEEP_ALIVE, 16#0).
-define(PROTO_LOGIN_REQUEST, 16#1).
-define(PROTO_HANDSHAKE, 16#2).
-define(PROTO_CHAT_MESSAGE, 16#3).
-define(PROTO_TIME_UPDATE, 16#4).
-define(PROTO_ENTITY_EQUIPMENT, 16#5).
-define(PROTO_SPAWN_POSITION, 16#6).
-define(PROTO_USE_ENTITY, 16#7).
-define(PROTO_UPDATE_HEALTH, 16#8).
-define(PROTO_RESPAWN, 16#9).
-define(PROTO_PLAYER, 16#A).
-define(PROTO_PLAYER_POSITION, 16#B).
-define(PROTO_PLAYER_LOOK, 16#C).
-define(PROTO_PLAYER_POSITION_LOOK, 16#D).
-define(PROTO_PLAYER_DIGGING, 16#E).
-define(PROTO_PLAYER_BLOCK_PLACEMENT, 16#F).
-define(PROTO_HOLDING_CHANGE, 16#10).
-define(PROTO_USE_BED, 16#11).
-define(PROTO_ANIMATION, 16#12).
-define(PROTO_ENTITY_ACTION, 16#13).
-define(PROTO_NAMED_ENTITY_SPAWN, 16#14).
-define(PROTO_PICKUP_SPAWN, 16#15).
-define(PROTO_COLLECT_ITEM, 16#16).
-define(PROTO_ADD_OBJECT, 16#17).
-define(PROTO_MOB_SPAWN, 16#18).
-define(PROTO_ENTITY_PAINTING, 16#19).
-define(PROTO_404, 16#1B).
-define(PROTO_ENTITY_VELOCITY, 16#1C).
-define(PROTO_DESTROY_ENTITY, 16#1D).
-define(PROTO_ENTITY, 16#1E).
-define(PROTO_ENTITY_RELATIVE_MOVE, 16#1F).
-define(PROTO_ENTITY_LOOK, 16#20).
-define(PROTO_ENTITY_LOOK_RELATIVE_MOVE, 16#21).
-define(PROTO_ENTITY_TELEPORT, 16#22).
-define(PROTO_ENTITY_STATUS, 16#26).
-define(PROTO_ATTACH_ENTITY, 16#27).
-define(PROTO_ENTITY_METADATA, 16#28).
-define(PROTO_PRE_CHUNK, 16#32).
-define(PROTO_MAP_CHUNK, 16#33).
-define(PROTO_MULTI_BLOCK_CHANGE, 16#34).
-define(PROTO_BLOCK_CHANGE, 16#35).
-define(PROTO_PLAY_NOTE_BLOCK, 16#36).
-define(PROTO_EXPLOSION, 16#3C).
-define(PROTO_NEW_INVALID_STATE, 16#3C).
-define(PROTO_WEATHER, 16#47).
-define(PROTO_OPEN_WINDOW, 16#64).
-define(PROTO_CLOSE_WINDOW, 16#65).
-define(PROTO_WINDOW_CLICK, 16#66).
-define(PROTO_SET_SLOT, 16#67).
-define(PROTO_WINDOW_ITEMS, 16#68).
-define(PROTO_UPDATE_PROGRESS_BAR, 16#69).
-define(PROTO_TRANSACTION, 16#6A).
-define(PROTO_UPDATE_SIGN, 16#82).
-define(PROTO_INCREMENT_STATISTIC, 16#C8).
-define(PROTO_DISCONNECT, 16#FF).


-record(srv_keep_alive, {
    packet_id = ?PROTO_KEEP_ALIVE
    }).

-record(srv_login_response, {
      packet_id = ?PROTO_LOGIN_RESPONSE,
      player_entity_id,
      unused_string,
      map_seed,
      dimension
     }).

-record(srv_handshake, {
      packet_id = ?PROTO_HANDSHAKE,
      connection_hash
     }).

-record(srv_chat_message, {
      packet_id = ?PROTO_CHAT_MESSAGE,
      message
     }).

-record(srv_inventory, {
      packet_id = ?PROTO_INVENTORY,
      player_entity_id,
      slot_id,
      item_id,
      unknown
     }).
      
-record(srv_disconnect, {
    packet_id = ?PROTO_DISCONNECT,
    reason
}).
