%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 11月 2019 18:16
%%%-------------------------------------------------------------------
-module(user_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/0, add_user/3, delete_user/1, delete_user/2, list_users/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-include("../include/user.hrl").
-define(SERVER, ?MODULE).

-record(state, {user_table}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_user(PName, UserName, RoomNum) ->
  gen_server:call(?MODULE, {add_uesr, PName, UserName, RoomNum}).
delete_user(PName) ->
  gen_server:call(?MODULE, {delete_user, PName}).
delete_user(PName, RoomNum) ->
  gen_server:call(?MODULE, {delete_user, PName, RoomNum}).
list_users(RoomNum) ->
  gen_server:call(?MODULE, {list_active_users, RoomNum}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{user_table = ets:new(room_users, [duplicate_bag, {keypos, #user.p_name}])}}.

%% 添加某个房间的用户
handle_call({add_uesr, PName, UserName, RoomNum}, _From, State) ->
  % 检查房间内是否有用户名重复
  case check_user_exist(State#state.user_table, UserName, RoomNum) of
    % 不存在则执行添加
    undefined ->
      % 检查该连接是否已连接到该房间
      case check_pname_in_room(State#state.user_table, PName, RoomNum) of
        undefined ->
          ets:insert(State#state.user_table, #user{name = UserName, p_name = PName, roomNum = RoomNum}),
          {reply, {ok, UserName, RoomNum}, State};
        _ ->
          {reply, {error, "PName name had in room."}, State}
      end;
    _ ->
      {reply, {error, "User name had registered."}, State}
  end;
%% 删除某个连接的所有用户
handle_call({delete_user, PName}, _From, State) ->
  handle_call({delete_user, PName, '_'}, _From, State);
%% 删除某个连接在某个房间的用户
handle_call({delete_user, PName, RoomNum}, _From, State) ->
  Result = case ets:match_object(State#state.user_table, #user{p_name = PName, roomNum = RoomNum, _ = '_'}) of
             [] ->
               {error, undefinded};
             Other ->
               lists:foreach(fun(User) -> ets:delete_object(State#state.user_table, User) end, Other),
               {ok, PName, RoomNum}
           end,
  {reply, Result, State};
%% 查询房间内可用用户列表
handle_call({list_active_users, RoomNum}, _From, State) ->
  {reply, get_user_list(RoomNum, State#state.user_table), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% 校验用户名为UserName的用户是否已在房间中
check_user_exist(Table, UserName, RoomNum) ->
  case ets:match(Table, #user{name = UserName, roomNum = RoomNum, _ = '_'}) of
    [X] -> X;
    _ -> undefined
  end.
check_pname_in_room(Table, PName, RoomNum) ->
  case ets:match(Table, #user{p_name = PName, roomNum = RoomNum, _ = '_'}) of
    [X] -> X;
    _ -> undefined
  end.
% 获得某个房间的用户列表
get_user_list(RoomNum, Table) ->
  ets:match_object(Table, #user{roomNum = RoomNum, _ = '_'}).

