-module(ceug_list).

-export([start/0, stop/0, restart/0]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

restart() ->
    stop(),
    start().
