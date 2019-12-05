%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 29. 11月 2019 20:04
%%%-------------------------------------------------------------------
-module(user_server).
-author("MMMGDZL").

-behaviour(gen_server).

%% API
-export([start_link/0, add_user/4, delete_user/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-include("../include/record.hrl").
-define(SERVER, ?MODULE).

-record(state, {user_table}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 添加用户信息
add_user(UserId, Username, Avatar, PName) ->
    gen_server:cast(?MODULE, {add_uesr, UserId, Username, Avatar, PName}).
%% 删除用户信息
delete_user(UserId) ->
    gen_server:cast(?MODULE, {delete_user, UserId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{user_table = ets:new(room_users, [set, named_table, {keypos, #user.id}])}}.

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
    {reply, _From, State}.

%% handle_cast
%% 保存用户信息
do_handle_cast({add_uesr, UserId, Username, Avatar, PName}, State) ->
    ets:insert(State#state.user_table, #user{id = UserId, name = Username, avatar = Avatar, p_name = PName}),
    {noreply, State};
%% 删除某个用户
do_handle_cast({delete_user, UserId}, State) ->
    ets:delete(State#state.user_table, UserId),
    {noreply, State};
do_handle_cast(Request, State) ->
    ?UN_HANDLE_CAST(Request),
    {noreply, State}.

%% handle_info
do_handle_info(Info, State) ->
    ?UN_HANDLE_INFO(Info),
    {noreply, State}.
