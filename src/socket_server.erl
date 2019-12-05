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
-export([start_link/3, send_message/2, encode_and_send_message/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).
-record(state, {player_p_name, socket, p_name}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Socket, RegisterName, UserId) ->
    gen_server:start_link({local, RegisterName}, ?MODULE, [Socket, RegisterName, UserId], []).

%% 发送消息
send_message(PName, Message) ->
    gen_server:cast(PName, {send_message, Message}).
%% 协议编码并发送消息
encode_and_send_message(PName, Data) ->
    AgreeNumber = get_agree_number(Data),
    gen_server:cast(PName, {encode_and_send_message, AgreeNumber, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Socket, RegisterName, UserId]) ->
    {ok, #state{socket = Socket, p_name = RegisterName, player_p_name = list_to_atom("player_server_" ++ integer_to_list(UserId))}}.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle call cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Request, State]),
            {reply, From, State}
    end.

handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
       Type : Reason ->
           error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Request, State]),
           {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        Type : Reason ->
            error_logger:error_msg("~p handle cast cause ~p:~p with ~p ~p~n", [State#state.p_name, Type, Reason, Info, State]),
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
    {reply, _From, State}.

%% handle_cast
do_handle_cast({send_message, Message}, State) ->
    gen_tcp:send(State#state.socket, Message),
    {noreply, State};
do_handle_cast({encode_and_send_message, AgreeNumber, Data}, State) ->
    gen_tcp:send(State#state.socket, lib_msg:encode_msg(AgreeNumber, Data)),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% handle_info
%% 客户端发送消息
do_handle_info({tcp, _Socket, Bin}, State) ->
    %路由执行业务
    try
        Result = lib_msg:decode_msg(Bin),
        io:format("~p~n", [Result]),
        route(Result,  State)
    catch
        _:X ->
            error_logger:error_msg("~p receive illegal binary : ~p cause ~p~n", [State#state.p_name, Bin, X]),
            {noreply, State}
    end;
% 客户端关闭连接
do_handle_info({tcp_closed, _Socket}, State) ->
    player_server:user_close_socket(State#state.player_p_name),
    %关闭本进程
    {stop, "Client socket closed.", State};
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.

%% 用户保存信息协议
route({0, Data}, State) ->
    player_server:user_save_info(State#state.player_p_name, Data),
    % 更新本进程状态
    {noreply, State};
%% 用户创建房间协议
route({1, Data}, State) ->
    % 执行创建房间
    player_server:create_room(State#state.player_p_name, Data),
    {noreply, State};
%% 用户加入房间协议
route({2, Data}, State) ->
    player_server:user_enter_room(State#state.player_p_name, Data),
    {noreply, State};
%% 用户退出房间协议
route({3, Data}, State) ->
    player_server:user_exit_room(State#state.player_p_name, Data),
    {noreply, State};
%% 获取房间列表协议
route({4, Data}, State) ->
    player_server:list_rooms(State#state.player_p_name, Data),
    {noreply, State};
%% 用户发言协议
route({5, Data}, State) ->
    player_server:user_send_message(State#state.player_p_name, Data),
    {noreply, State};
route(Data, State) ->
    error_logger:error_msg("Unhandle route: ~p~n", [Data]),
    {noreply, State}.

% 获取数据对应的协议号
get_agree_number(Data) ->
    if
        is_record(Data, s2c_user_save_info_0) -> 0;
        is_record(Data, s2c_create_room_1) -> 1;
        is_record(Data, s2c_user_enter_room_2) -> 2;
        is_record(Data, s2c_user_exit_room_3) -> 3;
        is_record(Data, s2c_list_rooms_4) -> 4;
        is_record(Data, s2c_user_send_message_5) -> 5;
        true -> undefined
    end.
