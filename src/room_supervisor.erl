%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(room_supervisor).
-behaviour(supervisor).
%% API
-export([start_link/1, new_room_server/4]).
%% 监控器回调函数
-export([init/1]).
-define(SERVER, ?MODULE).
-define(COUNT, count).
%%%===================================================================
%%% API函数
%%%===================================================================
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

new_room_server(RoomId, RoomName, RegisterName, Permanent) ->
    % 挂载一个监听节点到本监控树上
    RoomServerConfig = {RegisterName,
        {room_server, start_link, [RoomId, RoomName, RegisterName, Permanent]},
        temporary, 5000, worker, dynamic},
    supervisor:start_child(?SERVER, RoomServerConfig).

%%%===================================================================
%%% 监控器回调函数
%%%===================================================================
init([]) ->
    %启动子监听服务
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    ChildList = [
        {room_handler, {room_handler, start_link, []}, Restart, Shutdown, Type, [room_handler]}
    ],
    {ok, {SupFlags, ChildList}}.
%%%===================================================================
%%% 内部函数
%%%===================================================================