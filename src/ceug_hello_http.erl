-module(ceug_hello_http).

-export([start_link/0]).

-define(port, 8336).

start_link() ->
    psycho_server:start_link(?port, fun app/1).

app(_Env) ->
    {{200, "OK"}, [{"Content-Type", "text/plain"}], "Hello!"}.
