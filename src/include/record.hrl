%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 25. 11月 2019 18:27
%%%-------------------------------------------------------------------
-author("MMMGDZL").
-define(WORLD_ROOM, "WORLD").
-define(WORLD_ROOM_ID, "1").

-record(user, {id, name, avatar, p_name}).
-record(room, {id, name, p_name, permanent}).
%% c2s s2c
%% 用户注册协议
-record(state_0, {username, avatar}).
-record(state_0_echo, {result_type, user_id, world_room_id}).
%% 创建房间协议
-record(state_1, {room_name}).
-record(state_1_echo, {result_type, reason, room_id, room_name}).
%% 加入房间协议
-record(state_2, {room_id}).
-record(state_2_echo, {result_type, reason, room_id, room_name}).
%% 退出房间协议
-record(state_3, {room_id}).
-record(state_3_echo, {result_type, reason, room_id}).
%% 获取房间列表协议
-record(state_4_echo, {room_list}).
%% 发送消息协议
-record(state_5, {message, room_id}).
-record(state_5_cast, {user_id, user_name, avatar, message, room_id, timestamp}).