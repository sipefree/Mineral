%%% @doc
%%% Records for messages (Client to Server packets)
%%% @end
-include("server_packets.hrl").

-record(cli_keep_alive, {
      packet_id = ?PROTO_KEEP_ALIVE
     }).

-record(cli_login_request, {
      packet_id = ?PROTO_LOGIN_REQUEST,
      protocol_version = 11,
      username,
      map_seed,
      dimension
     }).

-record(cli_handshake, {
      packet_id = ?PROTO_HANDSHAKE,
      username
     }).

-record(cli_chat_message, {
      packet_id = ?PROTO_CHAT_MESSAGE,
      message
     }).

