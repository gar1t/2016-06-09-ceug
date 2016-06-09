-module(ceug_list_index_http).

-export([create_app/0, handle/2]).

create_app() ->
    psycho_util:dispatch_app({?MODULE, handle}, [method, parsed_query_string]).

%% ===================================================================
%% Handle request
%% ===================================================================

handle("GET", Params) ->
    handle_action(action(Params), Params);
handle(_, _Params) ->
    ceug_list_http:bad_request().

action(Params) ->
    try_action_params(
      [{"add",    add},
       {"update", update},
       {"delete", delete},
       {"order",  order}],
      view, Params).

try_action_params([{Param, Val}|Rest], Default, Params) ->
    case lists:keymember(Param, 1, Params) of
        true -> Val;
        false -> try_action_params(Rest, Default, Params)
    end;
try_action_params([], Default, _Params) ->
    Default.

%% ===================================================================
%% Dispatch action
%% ===================================================================

handle_action(view, Params) ->
    ceug_list_http:ok_html(index_page(param_vars(Params)));
handle_action(add, Params) ->
    handle_add_item(add_item(validate_add_params(Params)));
handle_action(update, Params) ->
    handle_update_item(update_item(validate_update_params(Params)));
handle_action(delete, Params) ->
    handle_delete_item(delete_item(validate_delete_params(Params)));
handle_action(order, Params) ->
    handle_order_items(order_items(order_from_params(Params))).

%% ===================================================================
%% View
%% ===================================================================

param_vars(Params) ->
    try_page_message_params(
      [{"added",   "Item added"},
       {"updated", "Item updated"},
       {"deleted", "Item deleted"}],
       Params).

try_page_message_params([{Param, Msg}|Rest], Params) ->
    case lists:keymember(Param, 1, Params) of
        true -> [{page_message, Msg}];
        false -> try_page_message_params(Rest, Params)
    end;
try_page_message_params([], _Params) ->
    [].

index_page(ExtraVars) ->
    Vars = ExtraVars ++ ceug_list_http:global_vars() ++ page_vars(),
    ceug_list_page:render(ceug_index_page, Vars).

page_vars() ->
    [{items, items()}].

items() ->
    [[{id, Id}|Attrs] || {Id, Attrs} <- ceug_list_db:items()].

%% ===================================================================
%% Add
%% ===================================================================

validate_add_params(Params) ->
    Schema =
        [{"name", [{min_length, 1}]},
         {"desc", []}],
    psycho_util:validate(Params, Schema).

add_item({ok, Params}) ->
    Name = proplists:get_value("name", Params),
    Desc = proplists:get_value("desc", Params),
    ceug_list_db:add_item(Name, Desc);
add_item({error, {"name", _}}) ->
    {error, "What would you like to add?"}.

handle_add_item({ok, _NewItemId}) ->
    ceug_list_http:redirect("/?added");
handle_add_item({error, Message}) ->
    ceug_list_http:ok_html(index_page([{page_message, Message}])).

%% ===================================================================
%% Update
%% ===================================================================

validate_update_params(Params) ->
    Schema =
        [{"id",   [required, integer]},
         {"name", [{min_length, 1}]},
         {"desc", []}],
    psycho_util:validate(Params, Schema).

update_item({ok, Params}) ->
    Id = proplists:get_value("id", Params),
    Name = proplists:get_value("name", Params),
    Desc = proplists:get_value("desc", Params),
    ceug_list_db:update_item(Id, Name, Desc);
update_item({error, _}) ->
    error.

handle_update_item(ok) ->
    ceug_list_http:redirect("/?updated");
handle_update_item(error) ->
    ceug_list_http:redirect("/").

%% ===================================================================
%% Delete
%% ===================================================================

validate_delete_params(Params) ->
    Schema = [{"id", [required, integer]}],
    psycho_util:validate(Params, Schema).

delete_item({ok, Params}) ->
    Id = proplists:get_value("id", Params),
    ceug_list_db:delete_item(Id);
delete_item({error, _}) ->
    error.

handle_delete_item(ok) ->
    ceug_list_http:redirect("/?deleted");
handle_delete_item(error) ->
    ceug_list_http:redirect("/").

%% ===================================================================
%% Order
%% ===================================================================

order_from_params(Params) ->
    Param = proplists:get_value("order", Params, ""),
    [list_to_integer(D) || D <- parse_digits(Param)].

parse_digits(Str) ->
    case re:run(Str, "(\\d+)", [{capture, all_but_first, list}, global]) of
        {match, Match} -> [D || [D] <- Match];
        nomatch -> []
    end.

order_items(Order) ->
    ceug_list_db:order_items(Order).

handle_order_items(ok) ->
    ceug_list_http:redirect("/").
