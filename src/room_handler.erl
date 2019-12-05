%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(room_handler).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/0, user_send_message/3, create_room/4, delete_room/1, list_rooms/1, user_enter_room/3, user_exit_room/3, remove_user_completely/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-define(COUNT, count).
-record(room_user, {user_id, room_id, room_p_name}).
-record(state, {room_table, room_user_table}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%% 用户发言
user_send_message(UserId, RoomId, EncodeMsg) ->
    gen_server:cast(?MODULE, {user_send_message, UserId, RoomId, EncodeMsg}).
%% 用户进入房间
user_enter_room(UserId, UserPName, RoomId) ->
    gen_server:cast(?MODULE, {user_enter_room, UserId, UserPName, RoomId}).
%% 用户离开房间
user_exit_room(UserId, UserPName, RoomId) ->
    gen_server:cast(?MODULE, {user_exit_room, UserId, UserPName, RoomId}).
%% 创建房间
create_room(UserPName, RoomId, RoomName, Permanent) ->
    gen_server:cast(?MODULE, {create_room, UserPName, RoomId, RoomName, Permanent}).
%% 删除房间
delete_room(RoomId) ->
    gen_server:cast(?MODULE, {delete_room, RoomId}).
%% 展示房间列表
list_rooms(UserPName) ->
    gen_server:cast(?MODULE, {list_rooms, UserPName}).
%% 完全移除用户
remove_user_completely(UserId) ->
    gen_server:cast(?MODULE, {remove_user_completely, UserId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
    %初始化房间统计数
    put(?COUNT, 1),
    %预约创建世界聊天
    create_room(undefined, ?WORLD_ROOM_ID, ?WORLD_ROOM, true),
    {ok, #state{room_table = ets:new(room_table, [set, named_table, {keypos, #room.id}]),
                            room_user_table = ets:new(room_user_table, [bag, named_table, {keypos, #room_user.user_id}])}}.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle call cause ~p:~p with ~p ~p~n", [?MODULE, Type, Reason, Request, State]),
            {reply, From, State}
    end.

handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [?MODULE, Type, Reason, Request, State]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [?MODULE, Type, Reason, Info, State]),
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
do_handle_cast({user_send_message, UserId, RoomId, EncodeMsg}, State) ->
    % 寻找房间
    case ets:lookup(State#state.room_table, RoomId) of
        [Room] ->
            % 房间发言
            room_server:user_send_message(UserId, EncodeMsg, Room#room.p_name);
        _ ->
            error_logger:error_msg("User:~p send message:~p to room:~p failed, room not found~n", [UserId, EncodeMsg, RoomId])
    end,
    {noreply, State};
do_handle_cast({user_enter_room, UserId, UserPName, RoomId}, State) ->
    % 寻找房间
    case ets:lookup(State#state.room_table, RoomId) of
        [Room] ->
            % 添加房间中的用户记录
            room_server:add_user(UserId, UserPName, Room#room.p_name),
            % 记录用户所在房间
            ets:insert(State#state.room_user_table, #room_user{room_id = RoomId, room_p_name = Room#room.p_name, user_id = UserId}),
            player_server:user_enter_room_callback(UserPName, {ok, Room});
        _ ->
            error_logger:error_msg("User:~p enter room:~p failed, room not found~n", [UserId, RoomId]),
            player_server:user_enter_room_callback(UserPName, {error, "Room not found"})
    end,
    {noreply, State};
do_handle_cast({user_exit_room, UserId, UserPName, RoomId}, State) ->
    % 寻找用户对应房间的记录
    case ets:match_object(State#state.room_user_table, #room_user{room_id = RoomId, user_id = UserId, _ = '_'}) of
        [RoomUser] ->
            % 删除用户
            ets:delete_object(State#state.room_user_table, RoomUser),
            % 通知房间删除用户
            room_server:delete_user(UserId, RoomUser#room_user.room_p_name),
            % 调用回调
            player_server:user_exit_room_callback(UserPName, {ok, #room{id = RoomUser#room_user.room_id}});
        _ ->
            error_logger:error_msg("User:~p exit room:~p failed, room not found~n", [UserId, RoomId]),
            player_server:user_exit_room_callback(UserPName, {error, "User not in the room"})
    end,
    {noreply, State};
do_handle_cast({create_room, UserPName, RoomId, RoomName, Permanent}, State) ->
    % 获取当前进程字典统计数
    Count = get(?COUNT),
    % 更新进程字典统计数
    put(?COUNT, Count + 1),
    % 创建注册名
    RegisterName = list_to_atom("room_server_" ++ integer_to_list(Count)),
    % 创建房间进程
    CallbackMsg = case room_supervisor:new_room_server(RoomId, RoomName, RegisterName, Permanent) of
                      {ok, _} ->
                          % 房间进程创建成功, 插入数据
                          ets:insert(State#state.room_table, #room{id = RoomId, name = RoomName, p_name = RegisterName, permanent = Permanent}),
                          {ok, #room{id = RoomId, name = RoomName}};
                      Result ->
                          % 失败则添加错误日志
                          error_logger:error_msg("Create room failed, count:~p, name:~p result:~p~n", [Count, RoomName, Result]),
                          {error, "Create room failed"}
                  end,
    if
    %如果是用户创建房间则返回创建房间结果
        UserPName /= undefined ->
            player_server:create_room_callback(UserPName, CallbackMsg);
        true -> true
    end,
    {noreply, State};
do_handle_cast({delete_room, RoomId}, State) ->
    ets:delete(State#state.room_table, RoomId),
    {noreply, State};
do_handle_cast({list_rooms, UserPName}, State) ->
    player_server:list_rooms_callback(UserPName, ets:tab2list(State#state.room_table)),
    {noreply, State};
do_handle_cast({remove_user_completely, UserId}, State) ->
    % 获取用户所在的所有房间并执行退出
    lists:foreach(fun(RoomUser) ->
        catch room_server:delete_user(UserId, RoomUser#room_user.room_p_name)
                  end, ets:lookup(State#state.room_user_table, UserId)),
    ets:delete(State#state.room_user_table, UserId),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% handle_info
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.