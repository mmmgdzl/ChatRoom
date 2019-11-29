%%%-------------------------------------------------------------------
%%% @作者XXX<me@hostname.local>
%%% @版权所有 (C) 2013, XXX
%%% @doc
%%%
%%% @end
%%% 创建于：2013年5月26日 作者XXX <me@hostname.local>
%%%-------------------------------------------------------------------
-module(chat_server).
-behaviour(gen_server).
%% API
-export([start_link/0, send_message/1]).
%% gen_server回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-include("../include/user.hrl").
-define(SERVER, ?MODULE).
-record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Message为编码后的消息
send_message([UserName, Message, RoomNum]) ->
  gen_server:cast(?MODULE, {send_message, UserName, Message, RoomNum}).

%%%===================================================================
%%% gen_server回调函数
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = callOK,
  {reply, Reply, State}.

% 广播消息
handle_cast({send_message, UserName, Message, RoomNum}, State) ->
  % 获取房间内的用户列表
  UserLists = user_server:list_users(RoomNum),
  % 逐个广播(发送者不广播)
  lists:foreach(
    fun(User) ->
      if
        User#user.name /= UserName ->
          socket_server:send_message(User#user.p_name, Message);
        true -> true
      end
    end, UserLists),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%%===================================================================
%%% 内部函数
%%%===================================================================