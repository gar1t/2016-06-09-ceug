-module(ceug_list_app).

-export([init/0]).

init() ->
    psycho_mime:init(),
    ceug_list_page:compile_templates(),
    {ok, children()}.

children() ->
    [ceug_list_db, ceug_list_http, ceug_hello_http].
