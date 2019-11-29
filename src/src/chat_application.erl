%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 25. 11月 2019 16:53
%%%-------------------------------------------------------------------
-module(chat_application).
-author("MMMGDZL").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, StartArgs) ->
  chat_supervisor:start_link(StartArgs),
  socket_supervisor:start_link(StartArgs).

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
