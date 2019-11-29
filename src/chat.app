%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 25. 11月 2019 16:55
%%%-------------------------------------------------------------------
{
  application, chat,
  [
    {description, "The chat room"},
    {vsn, "1.0"},
    {modules, [chat_application, chat_supervisor, socket_supervisor, chat_server, user_server, socket_server, socket_listener]},
    {registered, [chat_server, user_server, socket_server, socket_listener]},
    {applications, [kernel, stdlib]},
    {mod, {chat_application, []}},
    {start_phases, []}
  ]
}.