%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(socket_listener).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-define(LISTEN_PORT, 7777).
-define(COUNT, count).
-record(state, {listen}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% 开始监听
start_accept() ->
    gen_server:cast(?MODULE, {start_accept}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    % 初始化进程字典统计数
    put(?COUNT, 1),
    % 开启监听端口
    {ok, Listen} = gen_tcp:listen(?LISTEN_PORT, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
    start_accept(),
    {ok, #state{listen = Listen}}.

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
do_handle_cast({start_accept}, State) ->
    Listen = State#state.listen,
    %开始监听
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            % 获取当前进程字典统计数
            Count = get(?COUNT),
            % 更新进程字典统计数
            put(?COUNT, Count + 1),
            % 连接建立成功则创建新socket_server并将Socket控制权限转移给新进程
            {ok, Pid} = socket_supervisor:new_socket_server(Socket, Count),
            gen_tcp:controlling_process(Socket, Pid),
            % 创建玩家进程
            player_supervisor:new_player_server(Count);
        Other ->
            error_logger:error_msg("Connect failed:~p~n", [Other])
    end,
    %进行下一轮监听
    start_accept(),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% handle_info
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.
