%%%-------------------------------------------------------------------
%%% @作者XXX <me@hostname.local>
%%% @版权所有 (C) 2013, XXX
%%% @doc
%%%
%%% @end
%%% 创建于：2013年5月26日 作者XXX <me@hostname.local>
%%%-------------------------------------------------------------------
-module(chat_supervisor).
-behaviour(supervisor).
%% API
-export([start_link/1]).
%% 监控器回调函数
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API函数
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 启动监控器
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).
%%%===================================================================
%%% 监控器回调函数
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 每当supervisor:start_link/[2,3]启动一个监控器时，新进程就会调用这个函数来确定重启策略、
%% 最大重启频率和子进程规范
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%% ignore |
%% {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  ChildList = [
    {chat_server, {chat_server, start_link, []}, Restart, Shutdown, Type, [chat_server]},
    {user_server, {user_server, start_link, []}, Restart, Shutdown, Type, [user_server]}
  ],
  {ok, {SupFlags, ChildList}}.
%%%===================================================================
%%% 内部函数
%%%===================================================================