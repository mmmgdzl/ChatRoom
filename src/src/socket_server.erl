%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 15:48
%%%-------------------------------------------------------------------
-module(socket_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/2, send_message/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-include("../include/user.hrl").

-define(SERVER, ?MODULE).
-record(state, {socket, p_name}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Socket, Name], []).

send_message(PName, Message) ->
  gen_server:cast(PName, {send_message, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, Name]) ->
  {ok, #state{socket = Socket, p_name = Name}}.

handle_call(_Request, _From, State) ->
  {reply, _From, State}.

handle_cast({send_message, Message}, State) ->
  gen_tcp:send(State#state.socket, Message),
  {noreply, State}.
% 客户端发送消息
handle_info({tcp, Socket, Bin}, State) ->
  %路由执行业务
  Result = lib_msg:decode_msg(Bin),
  route(Result, Socket, State);
% 客户端关闭连接
handle_info({tcp_closed, _Socket}, State) ->
  %删除用户表中的用户
  user_server:delete_user(State#state.p_name),
  %关闭本进程
  {stop, "Client socket closed.", State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 用户注册协议
route({0000 = SendState, [Name]}, Socket, State) ->
  % 注册用户
  CallBackMsg = case user_server:add_user(State#state.p_name, binary_to_list(Name), ?ROOM) of
    {ok, UserName, RoomNum} ->
      ["OK", UserName ++ " join " ++ RoomNum];
    {error, Reason} ->
      ["ERROR", Reason]
  end,
  io:format("~p~n", [CallBackMsg]),
  gen_tcp:send(Socket, lib_msg:encode_msg(SendState, CallBackMsg)),
  {noreply, State};
%% 用户发送信息协议
route({0001 = SendState, [UserName, Avatar, Message]}, _Socket, State) ->
  % 生成消息发送时间
  {{Year,Month,Day},{Hour,Minute,_}} = calendar:local_time(),
  Time = lists:flatten(io_lib:format("~p-~p-~p ~p:~p",[Year, Month, Day, Hour, Minute])),
  % 消息编码
  CallBackMessage = lib_msg:encode_msg(SendState,
    [binary_to_list(UserName), binary_to_list(Avatar),
      binary_to_list(Message), Time]),
  chat_server:send_message([binary_to_list(UserName), CallBackMessage, ?ROOM]),
  {noreply, State}.