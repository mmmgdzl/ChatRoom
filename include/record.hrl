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

-define(UN_HANDLE_CALL(Request), error_logger:error_msg("~p: un handle call :~p~n", [?MODULE, Request])).
-define(UN_HANDLE_CAST(Request), error_logger:error_msg("~p: un handle cast :~p~n", [?MODULE, Request])).
-define(UN_HANDLE_INFO(Request), error_logger:error_msg("~p: un handle info :~p~n", [?MODULE, Request])).

-record(user, {id, name, avatar, p_name}).
-record(room, {id, name, p_name, permanent}).

%% 用户注册协议
-record(c2s_user_save_info_0, {username, avatar}).
-record(s2c_user_save_info_0, {result_type, user_id, world_room_id}).
%% 创建房间协议
-record(c2s_create_room_1, {room_name}).
-record(s2c_create_room_1, {result_type, reason, room_id, room_name}).
%% 加入房间协议
-record(c2s_user_enter_room_2, {room_id}).
-record(s2c_user_enter_room_2, {result_type, reason, room_id, room_name}).
%% 退出房间协议
-record(c2s_user_exit_room_3, {room_id}).
-record(s2c_user_exit_room_3, {result_type, reason, room_id}).
%% 获取房间列表协议
-record(c2s_list_rooms_4, {room_list}).
-record(s2c_list_rooms_4, {room_list}).
%% 发送消息协议
-record(c2s_user_send_message_5, {message, room_id}).
-record(s2c_user_send_message_5, {user_id, user_name, avatar, message, room_id, timestamp}).