%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 11:52
%%%-------------------------------------------------------------------
-module(lib_msg).
-author("MMMGDZL").

%% API
-export([encode_msg/2, decode_msg/1]).

%% 编码
encode_msg(State, MessageList) ->
  lists:foldl(
    fun(Msg, Accumulation) ->
      B_Msg = list_to_binary(Msg),
      <<Accumulation/binary, (byte_size(B_Msg)):16, B_Msg/binary>>
    end, <<State:16>>, MessageList).

%% 解码
decode_msg(Binary) ->
  io:format("666: ~p~n", [Binary]),
  <<State:16, Data/binary>> = Binary,
  DataList = do_decode_msg(Data, []),
  {State, DataList}.

%% 执行解码
do_decode_msg(<<>>, Result) ->
  lists:reverse(Result);
do_decode_msg(Binary, Result) ->
  <<Size:16, Item:Size/binary, Data/binary>> = Binary,
  io:format("mmm : ~p~n", [binary_to_list(Item)]),
  do_decode_msg(Data, [Item] ++ Result).
