-module(ceug_list_http).

-export([start_link/0, global_vars/0, ok_html/1, not_found/0,
         bad_request/0, redirect/1]).

-define(port, 8333).

start_link() ->
    psycho_server:start_link(?port, create_app()).

%% ===================================================================
%% App / routes
%% ===================================================================

create_app() ->
    psycho_route:create_app(routes()).

routes() ->
    [{{starts_with, "/assets/"}, static_app()},
     {"/",                       ceug_list_index_http:create_app()}].

static_app() ->
    psycho_static:create_app("priv").

%% ===================================================================
%% Global vars
%% ===================================================================

global_vars() ->
    [{app,
      [{name, "CEUG Lists"},
       {desc, "A simple app for managing lists!"}]}].

%% ===================================================================
%% Response helpers
%% ===================================================================

ok_html(Html) ->
    {{200, "OK"}, [{"Content-Type", "text/html"}], Html}.

not_found() ->
    {{404, "Not Found"}, [{"Content-Type", "text/plain"}], "Not Found"}.

bad_request() ->
    {{400, "Bad Request"}, [{"Content-Type", "text/plain"}], "Bad Request"}.

redirect(Path) ->
    {{302, "Found"}, [{"Location", Path}], []}.
