%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 04. 12月 2019 15:17
%%%-------------------------------------------------------------------
-module(player_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/2, user_save_info/2, user_enter_room/2, user_exit_room/2, create_room/2, list_rooms/2, user_send_message/2,
    user_enter_room_callback/2, user_exit_room_callback/2, create_room_callback/2, list_rooms_callback/2, user_close_socket/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-record(state, {user_id, username, avatar, p_name, socket_p_name}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(RegisterName, Count) ->
    gen_server:start_link({local, RegisterName}, ?MODULE, [RegisterName, Count], []).

%% 用户注册信息
user_save_info(PName, Data) when is_record(Data, c2s_user_save_info_0) ->
    gen_server:cast(PName, {user_save_info, Data}).
%% 用户创建房间
create_room(PName, Data) when is_record(Data, c2s_create_room_1) ->
    gen_server:cast(PName, {create_room, Data}).
%% 用户进入房间
user_enter_room(PName, Data) when is_record(Data, c2s_user_enter_room_2) ->
    gen_server:cast(PName, {user_enter_room, Data}).
%% 用户退出房间
user_exit_room(PName, Data) when is_record(Data, c2s_user_exit_room_3) ->
    gen_server:cast(PName, {user_exit_room, Data}).
%% 展示房间列表
list_rooms(PName, Data) when is_record(Data, c2s_list_rooms_4) ->
    gen_server:cast(PName, {list_rooms, Data}).
%% 用户发言
user_send_message(PName, Data) when is_record(Data, c2s_user_send_message_5) ->
    gen_server:cast(PName, {user_send_message, Data}).
%% 用户关闭连接
user_close_socket(PName) ->
    gen_server:cast(PName, {user_close_socket}).
    
%% 用户进入房间回调
user_enter_room_callback(PName, Echo) ->
    gen_server:cast(PName, {user_enter_room_echo, Echo}).
%% 用户退出房间回调
user_exit_room_callback(PName, Echo) ->
    gen_server:cast(PName, {user_exit_room_echo, Echo}).
%% 用户创建房间回调
create_room_callback(PName, Echo) ->
    gen_server:cast(PName, {create_room_echo, Echo}).
%% 展示房间列表回调
list_rooms_callback(PName, Echo) ->
    gen_server:cast(PName, {list_rooms_echo, Echo}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([RegisterName, Count]) ->
    {ok, #state{user_id = Count, p_name = RegisterName, socket_p_name = list_to_atom("socket_server_" ++ integer_to_list(Count))}}.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle call cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Request, State]),
            {reply, From, State}
    end.

handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Request, State]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Info, State]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% handle_call
do_handle_call(Request, _From, State) ->
    ?UN_HANDLE_CALL(Request),
    {reply, ok, State}.

%% handle_cast
do_handle_cast({user_save_info, #c2s_user_save_info_0{username = Name, avatar = Avatar}}, State) ->
    % 保存用户信息
    user_server:add_user(State#state.user_id, Name, Avatar, State#state.p_name),
    CallBackMsg = #s2c_user_save_info_0{result_type = "ok", user_id = State#state.user_id, world_room_id = ?WORLD_ROOM_ID},
    % 将结果发送回用户
    socket_server:encode_and_send_message(State#state.socket_p_name, CallBackMsg),
    % 更新本进程状态
    {noreply, State#state{username = Name, avatar = Avatar}};
do_handle_cast({create_room, #c2s_create_room_1{room_name = RoomName}}, State) ->
    % 执行创建房间
    RoomId = uuid:get(),
    room_handler:create_room(State#state.p_name, RoomId, RoomName, false),
    {noreply, State};
do_handle_cast({create_room_echo, {ResultType, Result}}, State) ->
    CallBackMsg = case ResultType of
                      ok ->
                          #s2c_create_room_1{result_type = "ok", room_id = Result#room.id, room_name = Result#room.name};
                      error ->
                          #s2c_create_room_1{result_type = "error", reason = Result}
                  end,
    % 将结果发送回用户
    socket_server:encode_and_send_message(State#state.socket_p_name, CallBackMsg),
    {noreply, State};
do_handle_cast({user_enter_room, #c2s_user_enter_room_2{room_id = RoomId}}, State) ->
    room_handler:user_enter_room(State#state.user_id, State#state.p_name, RoomId),
    {noreply, State};
do_handle_cast({user_enter_room_echo, Echo}, State) ->
    CallBackMsg = case Echo of
                      {ok, Room}->
                          #s2c_user_enter_room_2{result_type = "ok", room_id = Room#room.id, room_name = Room#room.name};
                      {error, Reason} ->
                          #s2c_user_enter_room_2{result_type = "error", reason = Reason}
                  end,
    % 将结果发送回用户
    socket_server:encode_and_send_message(State#state.socket_p_name, CallBackMsg),
    {noreply, State};
do_handle_cast({user_exit_room, #c2s_user_exit_room_3{room_id = RoomId}}, State) ->
    room_handler:user_exit_room(State#state.user_id, State#state.p_name, RoomId),
    {noreply, State};
do_handle_cast({user_exit_room_echo, Echo}, State) ->
    CallBackMsg = case Echo of
                      {ok, Room}->
                          #s2c_user_exit_room_3{result_type = "ok", room_id = Room#room.id};
                      {error, Reason} ->
                          #s2c_user_exit_room_3{result_type = "error", reason = Reason}
                  end,
    % 将结果发送回用户
    socket_server:encode_and_send_message(State#state.socket_p_name, CallBackMsg),
    {noreply, State};
do_handle_cast({list_rooms, #c2s_list_rooms_4{}}, State) ->
    room_handler:list_rooms(State#state.p_name),
    {noreply, State};
do_handle_cast({list_rooms_echo, Echo}, State) ->
    % 将结果发送回用户
    socket_server:encode_and_send_message(State#state.socket_p_name, #s2c_list_rooms_4{room_list = Echo}),
    {noreply, State};
do_handle_cast({user_send_message, #c2s_user_send_message_5{message = Message, room_id = RoomId}}, State) ->
    %编码
    CastMsg = #s2c_user_send_message_5{user_id = State#state.user_id, user_name = State#state.username, avatar = State#state.avatar,
        message = Message, room_id = RoomId, timestamp = timestamp()},
    BCastMsg = lib_msg:encode_msg(5, CastMsg),
    room_handler:user_send_message(State#state.user_id, RoomId, BCastMsg),
    {noreply, State};
do_handle_cast({user_close_socket}, State) ->
    % 删除用户表中的用户
    user_server:delete_user(State#state.user_id),
    % 删除房间中的本用户
    room_handler:remove_user_completely(State#state.user_id),
    % 关闭本进程
    {stop, "Client socket closed.", State};
do_handle_cast({send_message, Message}, State) ->
    socket_server:send_message(State#state.socket_p_name, Message),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% handle_info
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.


%% 获取当前时间戳
timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.