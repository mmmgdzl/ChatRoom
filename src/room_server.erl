%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(room_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/4, add_user/3, delete_user/2, user_send_message/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-define(DESTROY_DELAY, 10000).
-record(room_user, {user_id, user_p_name}).
-record(state, {room_id, room_name, p_name, user_table, permanent, destroy}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(RoomId, RoomName, PName, Permanent) ->
    gen_server:start_link({local, PName}, ?MODULE, [RoomId, RoomName, PName, Permanent], []).

% 添加用户
add_user(UserId, UserPName, RoomPName) ->
    gen_server:cast(RoomPName, {add_user, UserId, UserPName}).
% 删除用户
delete_user(UserId, RoomPName) ->
    gen_server:cast(RoomPName, {delete_user, UserId}).
% 广播发言
user_send_message(UserId, EncodeMsg, RoomPName) ->
    gen_server:cast(RoomPName, {user_send_message, UserId, EncodeMsg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([RoomId, RoomName, PName, Permanent]) ->
    {ok, #state{room_id = RoomId, room_name = RoomName, p_name = PName,
        permanent = Permanent, user_table = ets:new(user_table, [set, {keypos, #room_user.user_id}])}}.

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
do_handle_cast({add_user, UserId, UserPName}, State) ->
    ets:insert(State#state.user_table, #room_user{user_id = UserId, user_p_name = UserPName}),
    % 新用户加入时如果在倒数计时则取消计时
    if
        State#state.destroy /= undefined ->
            erlang:cancel_timer(State#state.destroy),
            {noreply, State#state{destroy = undefined}};
        true -> {noreply, State}
    end;
do_handle_cast({delete_user, UserId}, State) ->
    % 删除用户
    ets:delete(State#state.user_table, UserId),
    % 查看用户数
    UserCount = erlang:length(ets:tab2list(State#state.user_table)),
    if
    % 当用户数为0且聊天室为非永久时倒数计时摧毁
        UserCount == 0, State#state.permanent == false ->
            TimerRef = erlang:send_after(?DESTROY_DELAY, self(), {destroy_room}),
            {noreply, State#state{destroy = TimerRef}};
        true -> {noreply, State}
    end;
do_handle_cast({user_send_message, UserId, EncodeMsg}, State) ->
    % 获取房间中的所有用户
    UserList = ets:tab2list(State#state.user_table),
    % 广播消息
    lists:foreach(fun(User) ->
        if
            User#room_user.user_id /= UserId ->
                socket_server:send_message(User#room_user.user_p_name, EncodeMsg);
            true -> true
        end
                  end, UserList),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% do_handle_info
do_handle_info({destroy_room}, State) ->
    room_handler:delete_room(State#state.room_id),
    {stop, "No user in room", State};
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.
