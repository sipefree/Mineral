-module(mineral_msg).

-include("../include/client_packets.hrl").
-include("../include/server_packets.hrl").
-include("../include/types.hrl").

-export([pack/1, unpack/1]).

-compile([debug_info]).

pack(#srv_login_response{
           packet_id = ID, 
           player_entity_id = PEID,
           unused_string = UUS,
           map_seed = MS,
           dimension = DIM
          }) ->
    UUSBin = erlang:list_to_binary(UUS),
    Size = erlang:size(UUSBin),
    UUSUCS = ?mc_ucs2(UUSBin),
    <<?mc_byte(ID), ?mc_int(PEID), ?mc_short(Size), UUSUCS/binary, ?mc_long(MS), ?mc_byte(DIM)>>;

pack(#srv_keep_alive{}) ->
    <<?mc_byte(0)>>;

pack(#srv_handshake{
          packet_id = ID,
          connection_hash = Hash
          }) ->
    HashBin = ?mc_ucs2(Hash),
    Size = erlang:size(Hash),
    <<?mc_byte(ID), ?mc_short(Size), HashBin/binary>>;

pack(#srv_chat_message{
          packet_id = ID,
          message = Message
          }) ->
    MessageBin = <<Message/binary>>,
    Size = erlang:size(MessageBin),
    <<?mc_byte(ID), ?mc_short(Size), MessageBin>>;
    
pack(#srv_inventory{
          packet_id = ID,
          player_entity_id = PEID,
          slot_id = SID,
          item_id = IID,
          unknown = Unknown
          }) ->
    <<?mc_byte(ID), ?mc_int(PEID), ?mc_short(SID), ?mc_short(IID), ?mc_short(Unknown)>>;

pack(_) ->
    error.

%-------------------------------------------------%

unpack(<<?mc_byte(PacketID), Rest/binary>>) ->
    case PacketID of
    0 ->
        #cli_keep_alive{};
    1 ->
        <<?mc_int(ProtocolVersion), ?mc_short(UsernameLength), RRest/binary>> = Rest,
        ULength = 2*UsernameLength,
        <<UsernameBin:ULength/binary, RRRest/binary>> = RRest,
        Username = unicode:characters_to_list(UsernameBin, utf16),
        <<?mc_long(MapSeed), ?mc_byte(Dimension)>> = RRRest,
        #cli_login_request{
            protocol_version = ProtocolVersion,
            username = Username,
            map_seed = MapSeed,
            dimension = Dimension
        };
    2 ->
        <<?mc_short(_Length), Username/binary>> = Rest,
        #cli_handshake{
            username = unicode:characters_to_list(Username, utf16)
        };
    3 ->
        <<?mc_short(_Length), Message/binary>> = Rest,
        #cli_chat_message{
            message = Message
        };
    _ ->
        {unknown, PacketID, Rest}
    end.

