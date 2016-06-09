-module(ceug_list_db).

-behavior(e2_service).

-export([start_link/0, items/0, add_item/2, delete_item/1,
         update_item/3, order_items/1]).

-export([init/1, handle_msg/3]).

-record(state, {items, order, next_id}).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

init([]) ->
    {ok, init_state()}.

init_state() ->
    Fake = fake_items(),
    Order = [Id || {Id, _} <- Fake],
    #state{
       items=dict:from_list(Fake),
       order=Order,
       next_id=lists:max(Order) + 1
      }.

fake_items() ->
    [{1,
      [{name, "Item 1"},
       {desc, "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur facilisis mauris ac lorem euismod, eget pellentesque dui lacinia. Morbi vel eros fringilla ante scelerisque condimentum vitae vitae tortor."}]},
     {2,
      [{name, "Item 2"},
       {desc, "Donec sem neque, tincidunt ut congue sed, auctor iaculis leo. Sed vitae ullamcorper lectus, sit amet placerat libero. Duis dignissim posuere dui sed porttitor."}]}
    ].

%% ===================================================================
%% API
%% ===================================================================

items() ->
    e2_service:call(?MODULE, items).

add_item(Name, Desc) ->
    e2_service:call(?MODULE, {add, Name, Desc}).

delete_item(Id) ->
    e2_service:call(?MODULE, {delete, Id}).

update_item(Id, Name, Desc) ->
    e2_service:call(?MODULE, {update, Id, Name, Desc}).

order_items(Order) ->
    e2_service:call(?MODULE, {order, Order}).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg(items, _From, State) ->
    handle_items(State);
handle_msg({add, Name, Desc}, _From, State) ->
    handle_add(Name, Desc, State);
handle_msg({delete, Id}, _From, State) ->
    handle_delete(Id, State);
handle_msg({update, Id, Name, Desc}, _From, State) ->
    handle_update(Id, Name, Desc, State);
handle_msg({order, Order}, _From, State) ->
    handle_order(Order, State).

%% ===================================================================
%% Items
%% ===================================================================

handle_items(#state{items=Items, order=Order}=State) ->
    Sorted = [{Id, dict:fetch(Id, Items)} || Id <- Order],
    {reply, Sorted, State}.

%% ===================================================================
%% Add
%% ===================================================================

handle_add(Name, Desc, State) ->
    Id = next_id(State),
    Attrs = [{name, Name}, {desc, Desc}],
    NextState = increment_next_id(add_item(Id, Attrs, State)),
    {reply, {ok, Id}, NextState}.

next_id(#state{next_id=Id}) -> Id.

add_item(Id, Attrs, #state{items=Items, order=Order}=S) ->
    S#state{
      items=dict:store(Id, Attrs, Items),
      order=Order++[Id]}.

increment_next_id(#state{next_id=Id}=S) ->
    S#state{next_id=Id + 1}.

%% ===================================================================
%% Delete
%% ===================================================================

handle_delete(Id, State) ->
    NextState = delete_item(Id, State),
    {reply, ok, NextState}.

delete_item(Id, #state{items=Items, order=Order}=S) ->
    S#state{
      items=dict:erase(Id, Items),
      order=lists:delete(Id, Order)}.

%% ===================================================================
%% Update
%% ===================================================================

handle_update(Id, Name, Desc, State) ->
    Attrs = [{name, Name}, {desc, Desc}],
    NextState = update_state_item(Id, Attrs, State),
    {reply, ok, NextState}.

update_state_item(Id, Attrs, #state{items=Items}=S) ->
    S#state{items=dict:store(Id, Attrs, Items)}.

%% ===================================================================
%% Order
%% ===================================================================

handle_order(New, #state{order=Old, items=Items}=State) ->
    Order = apply_new_order(New, Old, Items),
    NextState = set_order(Order, State),
    {reply, ok, NextState}.

apply_new_order(New, Old, Items) ->
    new_order_acc(New ++ Old, Items, []).

new_order_acc([Id|Rest], Items, Acc) ->
    case dict:is_key(Id, Items) of
        true -> new_order_acc(Rest, dict:erase(Id, Items), [Id|Acc]);
        false -> new_order_acc(Rest, Items, Acc)
    end;
new_order_acc([], Items, Acc) ->
    0 = dict:size(Items),
    lists:reverse(Acc).

set_order(Order, S) -> S#state{order=Order}.
