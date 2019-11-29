%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 11月 2019 15:49
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

-define(SERVER, ?MODULE).

-record(state, {listen}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  % 开启监听端口
  {ok, Listen} = gen_tcp:listen(7777, [binary, {packet, 1}, {reuseaddr, true}, {active, true}]),
  start_accept(),
  {ok, #state{listen = Listen}}.

% 开始监听
start_accept() ->
  gen_server:cast(?MODULE, {start_accept}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({start_accept}, State) ->
  Listen = State#state.listen,
  %开始监听
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      {ok, Pid} = socket_supervisor:new_socket_server(Socket),
      gen_tcp:controlling_process(Socket, Pid);
    _ ->
      error
  end,
  %进行下一轮监听
  start_accept(),
  {noreply, State};
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
