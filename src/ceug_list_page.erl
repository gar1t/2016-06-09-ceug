-module(ceug_list_page).

-export([compile_templates/0, render/1, render/2]).

-ifndef(recompile).
-define(recompile, true).
-endif.

compile_templates() ->
    compile_template("index.html", ceug_index_page),
    compile_template("debug.html", ceug_debug_page).

compile_template(Template, Mod) ->
    Path = filename:join("priv/templates", Template),
    {ok, _} = erlydtl:compile_file(Path, Mod, []).

render(Mod) ->
    render(Mod, []).

render(Mod, Vars) ->
    maybe_recompile_templates(?recompile),
    {ok, Page} = Mod:render(Vars),
    Page.

maybe_recompile_templates(true) -> compile_templates();
maybe_recompile_templates(false) -> ok.
