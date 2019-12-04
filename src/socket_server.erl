%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(socket_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/3, send_message/2, encode_and_send_message/3, user_enter_room_callback/2, create_room_callback/2, user_exit_room_callback/2, list_rooms_callback/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-record(state, {user_id, username, avatar, socket, p_name}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Socket, RegisterName, UserId) ->
    gen_server:start_link({local, RegisterName}, ?MODULE, [Socket, RegisterName, UserId], []).

%% 发送消息
send_message(PName, Message) ->
    gen_server:cast(PName, {send_message, Message}).
%% 协议编码并发送消息
encode_and_send_message(PName, State, Message) ->
    gen_server:cast(PName, {encode_and_send_message, State, Message}).

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
init([Socket, RegisterName, UserId]) ->
    {ok, #state{socket = Socket, p_name = RegisterName, user_id = UserId}}.

handle_call(Request, _From, State) ->
    ?UN_HANDLE_CALL(Request),
    {reply, _From, State}.

handle_cast({create_room_echo, {ResultType, Result}}, State) ->
    CallBackMsg = case ResultType of
                      ok ->
                          #s2c_create_room_1{result_type = "ok", room_id = Result#room.id, room_name = Result#room.name};
                      error ->
                          #s2c_create_room_1{result_type = "error", reason = Result}
                  end,
    % 将结果发送回用户
    io:format("~p~n", [CallBackMsg]),
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(1, CallBackMsg)),
    % 如果创建房间成功则本用户加入房间
    if
        ResultType == ok ->
            room_handler:user_enter_room(State#state.user_id, State#state.p_name, Result#room.id);
        true -> true
    end,
    {noreply, State};
handle_cast({user_enter_room_echo, Echo}, State) ->
    CallBackMsg = case Echo of
                      {ok, Room}->
                          #s2c_user_enter_room_2{result_type = "ok", room_id = Room#room.id, room_name = Room#room.name};
                      {error, Reason} ->
                          #s2c_user_enter_room_2{result_type = "error", reason = Reason}
                  end,
    % 将结果发送回用户
    io:format("~p~n", [CallBackMsg]),
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(2, CallBackMsg)),
    {noreply, State};
handle_cast({user_exit_room_echo, Echo}, State) ->
    CallBackMsg = case Echo of
                      {ok, Room}->
                          #s2c_user_exit_room_3{result_type = "ok", room_id = Room#room.id};
                      {error, Reason} ->
                          #s2c_user_exit_room_3{result_type = "error", reason = Reason}
                  end,
    % 将结果发送回用户
    io:format("~p~n", [CallBackMsg]),
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(3, CallBackMsg)),
    {noreply, State};
handle_cast({list_rooms_echo, Echo}, State) ->
    % 将结果发送回用户
    io:format("~p~n", [Echo]),
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(4, #s2c_list_rooms_4{room_list = Echo})),
    {noreply, State};
handle_cast({send_message, Message}, State) ->
    gen_tcp:send(State#state.socket, Message),
    {noreply, State};
handle_cast({encode_and_send_message, State, Message}, State) ->
    % 编码
    % 将结果发送回用户
    io:format("~p~n", [Message]),
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(State, Message)),
    {noreply, State}.

%% 客户端发送消息
handle_info({tcp, Socket, Bin}, State) ->
    %路由执行业务
    try
        Result = lib_msg:decode_msg(Bin),
        io:format("~p~n", [Result]),
        route(Result, Socket, State)
    catch
        _:X ->
            error_logger:error_msg("~p receive illegal binary : ~p cause ~p~n", [State#state.p_name, Bin, X]),
            {noreply, State}
    end;
% 客户端关闭连接
handle_info({tcp_closed, _Socket}, State) ->
    % 删除用户表中的用户
    user_server:delete_user(State#state.user_id),
    % 删除房间中的本用户
    room_handler:remove_user_completely(State#state.user_id),
    %关闭本进程
    {stop, "Client socket closed.", State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 获取当前时间戳
timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 用户注册协议
route({0 = SendState, #c2s_user_register_0{username = Name, avatar = Avatar}}, Socket, State) ->
    % 保存用户信息
    user_server:add_user(State#state.user_id, Name, Avatar, State#state.p_name),
    CallBackMsg = #s2c_user_register_0{result_type = "ok", user_id = State#state.user_id, world_room_id = ?WORLD_ROOM_ID},
    io:format("~p~n", [CallBackMsg]),
    % 加入世界聊天
    room_handler:user_enter_room(State#state.user_id, State#state.p_name, ?WORLD_ROOM_ID),
    % 将结果发送回用户
    gen_tcp:send(Socket, lib_msg:encode_msg(SendState, CallBackMsg)),
    % 更新本进程状态
    {noreply, State#state{username = Name, avatar = Avatar}};
%% 用户创建房间协议
route({1, #c2s_create_room_1{room_name = RoomName}}, _Socket, State) ->
    % 执行创建房间
    RoomId = uuid:get(),
    room_handler:create_room(State#state.p_name, RoomId, RoomName, false),
    {noreply, State};
%% 用户加入房间协议
route({2, #c2s_user_enter_room_2{room_id = RoomId}}, _Socket, State) ->
    room_handler:user_enter_room(State#state.user_id, State#state.p_name, RoomId),
    {noreply, State};
%% 用户退出房间协议
route({3, #c2s_user_exit_room_3{room_id = RoomId}}, _Socket, State) ->
    room_handler:user_exit_room(State#state.user_id, State#state.p_name, RoomId),
    {noreply, State};
%% 获取房间列表协议
route({4, _}, _Socket, State) ->
    room_handler:list_rooms(State#state.p_name),
    {noreply, State};
%% 用户发言协议
route({5, #c2s_user_send_message_5{message = Message, room_id = RoomId}}, _Socket, State) ->
    %编码
    CastMsg = #s2c_user_send_message_5{user_id = State#state.user_id, user_name = State#state.username, avatar = State#state.avatar,
        message = Message, room_id = RoomId, timestamp = timestamp()},
    BCastMsg = lib_msg:encode_msg(5, CastMsg),
    room_handler:user_send_message(State#state.user_id, RoomId, BCastMsg),
    {noreply, State};
route(Data, _Socket, State) ->
    error_logger:error_msg("Unhandle route: ~p~n", [Data]),
    {noreply, State}.
