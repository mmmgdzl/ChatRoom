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
    {modules, [chat_application,
      base_supervisor, socket_supervisor, room_supervisor,
      user_server, socket_server, room_server,
      socket_listener, room_handler]},
    {registered, [user_server, socket_server, socket_listener, room_handler]},
    {applications, [kernel, stdlib]},
    {mod, {chat_application, []}},
    {start_phases, []}
  ]
}.