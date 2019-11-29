%%%-------------------------------------------------------------------
%%% @作者XXX <me@hostname.local>
%%% @版权所有 (C) 2013, XXX
%%% @doc
%%%
%%% @end
%%% 创建于：2013年5月26日 作者XXX <me@hostname.local>
%%%-------------------------------------------------------------------
-module(socket_supervisor).
-behaviour(supervisor).
%% API
-export([start_link/1, new_socket_server/1]).
%% 监控器回调函数
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API函数
%%%===================================================================
start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

new_socket_server(Socket) ->
  % 挂载一个监听节点到本监控树上
  Name = list_to_atom("socket_server" ++ uuid:to_string_extra(uuid:v4())),
  SocketServerConfig = {Name,
    {socket_server, start_link, [Socket, Name]},
    temporary, 5000, worker, dynamic},
  supervisor:start_child(?SERVER, SocketServerConfig).

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
    {socket_listener, {socket_listener, start_link, []}, Restart, Shutdown, Type, [socket_listener]}
  ],
  {ok, {SupFlags, ChildList}}.
%%%===================================================================
%%% 内部函数
%%%===================================================================