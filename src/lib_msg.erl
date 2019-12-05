%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 11:52
%%%-------------------------------------------------------------------
-module(lib_msg).
-author("MMMGDZL").
-include("../include/record.hrl").

%% API
-export([encode_msg/2, safe_encode_msg/2, decode_msg/1, safe_decode_msg/1]).

%% 编码
encode_msg(0 = State, Echo) ->
    BResultType = encode_string(Echo#s2c_user_save_info_0.result_type),
    BWorldRoomId = encode_string(Echo#s2c_user_save_info_0.world_room_id),
    <<State:16, BResultType/binary, (Echo#s2c_user_save_info_0.user_id):16, BWorldRoomId/binary>>;
encode_msg(1 = State, Echo) ->
    BResultType = encode_string(Echo#s2c_create_room_1.result_type),
    case Echo#s2c_create_room_1.result_type of
        "ok" ->
            BRoomId = encode_string(Echo#s2c_create_room_1.room_id),
            BRoomName = encode_string(Echo#s2c_create_room_1.room_name),
            <<State:16, BResultType/binary, BRoomId/binary, BRoomName/binary>>;
        "error" ->
            BReason = encode_string(Echo#s2c_create_room_1.reason),
            <<State:16, BResultType/binary, BReason/binary>>
    end;
encode_msg(2 = State, Echo) ->
    BResultType = encode_string(Echo#s2c_user_enter_room_2.result_type),
    case Echo#s2c_user_enter_room_2.result_type of
        "ok" ->
            BRoomId = encode_string(Echo#s2c_user_enter_room_2.room_id),
            BRoomName = encode_string(Echo#s2c_user_enter_room_2.room_name),
            <<State:16, BResultType/binary, BRoomId/binary, BRoomName/binary>>;
        "error" ->
            BReason = encode_string(Echo#s2c_user_enter_room_2.reason),
            <<State:16, BResultType/binary, BReason/binary>>
    end;
encode_msg(3 = State, Echo) ->
    BResultType = encode_string(Echo#s2c_user_exit_room_3.result_type),
    case Echo#s2c_user_exit_room_3.result_type of
        "ok" ->
            BRoomId = encode_string(Echo#s2c_user_exit_room_3.room_id),
            <<State:16, BResultType/binary, BRoomId/binary>>;
        "error" ->
            BReason = encode_string(Echo#s2c_user_exit_room_3.reason),
            <<State:16, BResultType/binary, BReason/binary>>
    end;
encode_msg(4 = State, Echo) ->
    lists:foldl(fun(Room, Acc) ->
        BRoomId = encode_string(Room#room.id),
        BRoomName = encode_string(Room#room.name),
        <<Acc/binary, BRoomId/binary, BRoomName/binary>>
        end, <<State:16>>, Echo#s2c_list_rooms_4.room_list);
encode_msg(5 = State, Echo) ->
    BUserName = encode_string(Echo#s2c_user_send_message_5.user_name),
    BMessage = encode_string(Echo#s2c_user_send_message_5.message),
    BRoomId = encode_string(Echo#s2c_user_send_message_5.room_id),
    <<State:16, (Echo#s2c_user_send_message_5.user_id):16, BUserName/binary, (Echo#s2c_user_send_message_5.avatar):16, BMessage/binary, BRoomId/binary, (Echo#s2c_user_send_message_5.timestamp):32>>;
encode_msg(Other, Other2) ->
    error_logger:error_msg("Other encode : ~p ~p~n", [Other, Other2]),
    undefined.

%% 安全编码
safe_encode_msg(State, Echo) ->
    try
        {ok, encode_msg(State, Echo)}
    catch
        _ : _ ->
            error_logger:error_msg("Encode state:~p, echo:~p failed~n", [State, Echo]),
            error
    end.

%% 解码
decode_msg(Binary) ->
    io:format("666: ~p~n", [Binary]),
    <<State:16, Data/binary>> = Binary,
    {State, decode_map(State, Data)}.
%% 安全解码
safe_decode_msg(Binary) ->
    try
        {ok, decode_msg(Binary)}
    catch
        _ : _ ->
            error_logger:error_msg("Decode state:~p, echo:~p failed~n", [Binary]),
            error
    end.

%% 执行解码
decode_map(0, Binary) ->
    <<Size:16, BUsername:Size/binary, Binary2/binary>> = Binary,
    <<Avatar:16>> = Binary2,
    #c2s_user_save_info_0{username = binary_to_list(BUsername), avatar = Avatar};
decode_map(1, Binary) ->
    <<Size:16, BRoomName:Size/binary>> = Binary,
    #c2s_create_room_1{room_name = binary_to_list(BRoomName)};
decode_map(2, Binary) ->
    <<Size:16, BRoomId:Size/binary>> = Binary,
    #c2s_user_enter_room_2{room_id = binary_to_list(BRoomId)};
decode_map(3, Binary) ->
    <<Size:16, BRoomId:Size/binary>> = Binary,
    #c2s_user_exit_room_3{room_id = binary_to_list(BRoomId)};
decode_map(4, _Binary) ->
    #c2s_list_rooms_4{};
decode_map(5, Binary) ->
    <<Size:16, Message:Size/binary, Binary2/binary>> = Binary,
    <<Size2:16, RoomId:Size2/binary>> = Binary2,
    #c2s_user_send_message_5{message = binary_to_list(Message), room_id = binary_to_list(RoomId)};
decode_map(Other, Other2) ->
    error_logger:error_msg("Other decode : ~p ~p~n", [Other, Other2]).

%% 编码字符串
encode_string(String) ->
    BStr = list_to_binary(String),
    <<(byte_size(BStr)):16, BStr/binary>>.

