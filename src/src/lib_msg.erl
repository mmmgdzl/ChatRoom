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
-export([encode_msg/2, decode_msg/1]).

%% 编码
encode_msg(0 = State, Echo) ->
  BResultType = encode_string(Echo#state_0_echo.result_type),
  BWorldRoomId = encode_string(Echo#state_0_echo.world_room_id),
  <<State:16, BResultType/binary, (Echo#state_0_echo.user_id):16, BWorldRoomId/binary>>;
encode_msg(1 = State, Echo) ->
  BResultType = encode_string(Echo#state_1_echo.result_type),
  case Echo#state_1_echo.result_type of
    "ok" ->
      BRoomId = encode_string(Echo#state_1_echo.room_id),
      BRoomName = encode_string(Echo#state_1_echo.room_name),
      <<State:16, BResultType/binary, BRoomId/binary, BRoomName/binary>>;
    "error" ->
      BReason = encode_string(Echo#state_1_echo.reason),
      <<State:16, BResultType/binary, BReason/binary>>
  end;
encode_msg(2 = State, Echo) ->
  BResultType = encode_string(Echo#state_2_echo.result_type),
  case Echo#state_2_echo.result_type of
    "ok" ->
      BRoomId = encode_string(Echo#state_2_echo.room_id),
      BRoomName = encode_string(Echo#state_2_echo.room_name),
      <<State:16, BResultType/binary, BRoomId/binary, BRoomName/binary>>;
    "error" ->
      BReason = encode_string(Echo#state_2_echo.reason),
      <<State:16, BResultType/binary, BReason/binary>>
  end;
encode_msg(3 = State, Echo) ->
  BResultType = encode_string(Echo#state_3_echo.result_type),
  case Echo#state_3_echo.result_type of
    "ok" ->
      BRoomId = encode_string(Echo#state_3_echo.room_id),
      <<State:16, BResultType/binary, BRoomId/binary>>;
    "error" ->
      BReason = encode_string(Echo#state_3_echo.reason),
      <<State:16, BResultType/binary, BReason/binary>>
  end;
encode_msg(4 = State, Echo) ->
  lists:foldl(fun(Room, Acc) ->
    BRoomId = encode_string(Room#room.id),
    BRoomName = encode_string(Room#room.name),
    <<Acc/binary, BRoomId/binary, BRoomName/binary>>
    end, <<State:16>>, Echo#state_4_echo.room_list);
encode_msg(5 = State, Echo) ->
  io:format("~p~n", [Echo]),
  BUserName = encode_string(Echo#state_5_cast.user_name),
  BMessage = encode_string(Echo#state_5_cast.message),
  BRoomId = encode_string(Echo#state_5_cast.room_id),
  <<State:16, (Echo#state_5_cast.user_id):16, BUserName/binary, (Echo#state_5_cast.avatar):16, BMessage/binary, BRoomId/binary, (Echo#state_5_cast.timestamp):32>>.

%% 解码
decode_msg(Binary) ->
  io:format("666: ~p~n", [Binary]),
  <<State:16, Data/binary>> = Binary,
  {State, decode_map(State, Data)}.

%% 执行解码
decode_map(0, Binary) ->
  <<Size:16, BUsername:Size/binary, Binary2/binary>> = Binary,
  <<Avatar:16>> = Binary2,
  #state_0{username = binary_to_list(BUsername), avatar = Avatar};
decode_map(1, Binary) ->
  <<Size:16, BRoomName:Size/binary>> = Binary,
  #state_1{room_name = binary_to_list(BRoomName)};
decode_map(2, Binary) ->
  <<Size:16, BRoomId:Size/binary>> = Binary,
  #state_2{room_id = binary_to_list(BRoomId)};
decode_map(3, Binary) ->
  <<Size:16, BRoomId:Size/binary>> = Binary,
  #state_3{room_id = binary_to_list(BRoomId)};
decode_map(4, _Binary) ->
  undefined;
decode_map(5, Binary) ->
  <<Size:16, Message:Size/binary, Binary2/binary>> = Binary,
  <<Size2:16, RoomId:Size2/binary>> = Binary2,
  #state_5{message = binary_to_list(Message), room_id = binary_to_list(RoomId)};
decode_map(Other, Other2) ->
  io:format("break : ~p ~p", [Other, Other2]).

%% 编码字符串
encode_string(String) ->
  BStr = list_to_binary(String),
  <<(byte_size(BStr)):16, BStr/binary>>.

