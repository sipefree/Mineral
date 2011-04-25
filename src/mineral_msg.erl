-module(mineral_msg).

-include("../include/client_packets.hrl").
-include("../include/server_packets.hrl").
-include("../include/types.hrl").

-export([pack/1, unpack/1]).

pack(#login_response{
           packet_id = ID, 
           player_entity_id = PEID,
           unused_string = UUS,
           map_seed = MS,
           dimension = DIM
          }) ->
    UUSBin = <<UUS/binary>>,
    Size = erlang:size(UUSBin),
    <<?mc_byte(ID), ?mc_short(PEID), ?mc_short(Size), UUS/binary, ?mc_short(MS), ?mc_short(DIM)>>;

pack(#login_request{
           packet_id = ID,
           protocol_version = PVER,
           username = Username
          }) ->
    UsernameBin = <<Username/binary>>,
    Size = erlang:size(UsernameBin),
    <<?mc_byte(ID), ?mc_short(PVER), ?mc_short(Size), Username/binary>>;

pack(#handshake{
          packet_id = ID,
          connection_hash = Hash
          }) ->
    HashBin = erlang:list_to_binary(Hash),
    Size = erlang:size(HashBin),
    <<?mc_byte(ID), ?mc_short(Size), HashBin/binary>>;

pack(#chat_message{
          packet_id = ID,
          message = Message
          }) ->
    MessageBin = <<Message/binary>>,
    Size = erlang:size(MessageBin),
    <<?mc_byte(ID), ?mc_short(Size), MessageBin>>;

%-------------------------------------------------%

unpack(<<?mc_byte(PacketID), Rest/binary>>) ->
    case PacketID of
    1 ->
        <<?mc_int(ProtocolVersion), ?mc_short(UsernameLength), RRest/binary>> = Rest,
        ULength = 8*UsernameLength,
        <<Username:ULength/native-signed-integer>> = RRest,
        #login_request{
                protocol_version = ProtocolVersion,
                username = Username, 
        };
    2 ->
        <<?mc_short(_Length), Username/binary>> = Rest,
        #handshake{
            username = erlang:binary_to_list(Username)
        };
    3 ->
        <<?mc_short(_Length), Message/binary>> = Rest,
          #chat_message{
                 message = Message
          };
    _ ->
        error
    end.

