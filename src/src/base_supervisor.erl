%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(base_supervisor).
-behaviour(supervisor).
%% API
-export([start_link/1]).
%% 监控器回调函数
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API函数
%%%===================================================================
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).
%%%===================================================================
%%% 监控器回调函数
%%%===================================================================
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  ChildList = [
%%    {chat_server, {chat_server, start_link, []}, Restart, Shutdown, Type, [chat_server]},
    {user_server, {user_server, start_link, []}, Restart, Shutdown, Type, [user_server]}
  ],
  {ok, {SupFlags, ChildList}}.
%%%===================================================================
%%% 内部函数
%%%===================================================================