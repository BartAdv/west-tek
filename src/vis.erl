-module(vis).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2]).

-define(TEST, 1).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(vis_data, {vis_list=gb_trees:empty() %% Entity => Coord
                  ,grid=gb_trees:empty() %% Coord => Hex (rather terrible structure considering path tracing may come up)
                  ,pos
                  ,range=10}).

get_hex(Grid, Coords) ->
    case gb_trees:lookup(Coords, Grid) of
        {value, Hex} -> Hex;
        none -> sets:new()
    end.

add_entity(#vis_data{vis_list=VisList, grid=Grid}=Vis, Coords, Ent) ->
    Hex = sets:add_element(Ent, get_hex(Grid, Coords)),
    Grid2 = gb_trees:enter(Coords, Hex, Grid),
    VisList2 = gb_trees:insert(Ent, Coords, VisList),
    Vis#vis_data{vis_list=VisList2, grid=Grid2}.

remove_entity(#vis_data{vis_list=VisList, grid=Grid}=Vis, Coords, Ent) ->
    Hex = sets:del_element(Ent, get_hex(Grid, Coords)),
    Grid2 = gb_trees:enter(Coords, Hex, Grid),
    VisList2 = gb_trees:delete(Ent, VisList),
    Vis#vis_data{vis_list=VisList2, grid=Grid2}.

move_entity(#vis_data{grid=Grid}=Vis, From, To, Ent) ->
    HexFrom = sets:del_element(Ent, get_hex(Grid, From)),
    HexTo   = sets:add_element(Ent, get_hex(Grid, To)),
    Grid2   = gb_trees:enter(From, HexFrom, Grid),
    Grid3   = gb_trees:enter(To, HexTo, Grid2), %% don't judge me
    Vis#vis_data{grid=Grid3}.

dist({X1, Y1}, {X2, Y2}) ->
    math:sqrt((X1-X2)*(X1-X2) + ((Y1-Y2)*(Y1-Y2))).

%% gen_event

init({Coords, Range}) ->
    {ok, #vis_data{pos=Coords, range=Range}}.

handle_event({entity_entered_map, Ent, Coords, _Map},
             #vis_data{vis_list=VisList, grid=Grid, pos=Pos, range=Range}=Vis) ->
    case dist(Pos, Coords) =< Range of
        true ->
            Vis2 = add_entity(Vis, Coords, Ent),
            {ok, Vis2};
        false ->
            {ok, Vis}
    end;

handle_event({entity_left_map, Ent, _Map},
             #vis_data{vis_list=VisList, grid=Grid}=Vis) ->
    case gb_trees:lookup(Ent, VisList) of
        {value, Coords} ->
            Vis2 = remove_entity(Vis, Coords, Ent),
            {ok, Vis2};
        none -> {ok, Vis}
    end;

handle_event({entity_moved_on_map, Ent, From, To, _Map},
             #vis_data{vis_list=VisList, grid=Grid, pos=Pos, range=Range}=Vis) ->
    case gb_trees:lookup(Ent, VisList) of
        {value, Coords} ->
            case dist(Pos, To) =< Range of
                true ->
                    {ok, move_entity(Vis, From, To, Ent)};
                false ->
                    {ok, remove_entity(Vis, From, Ent)}
            end;
        none ->
            case dist(Pos, To) =< Range of
                true ->
                    {ok, add_entity(Vis, To, Ent)};
                false ->
                    {ok, Vis}
            end
    end.

handle_call({set_range, Range}, Vis) ->
    {ok, ok, Vis#vis_data{range=Range}};

handle_call({get, Coords}, #vis_data{grid=Grid}=Vis) ->
    Hex = get_hex(Grid, Coords),
    {ok, sets:to_list(Hex), Vis}.

-ifdef(TEST).

test_entity() ->
   spawn(fun()-> receive kill -> exit(killed) end end).

test_init(Coords, Range) ->
    {ok, Pid} = gen_event:start(),
    gen_event:add_handler(Pid, ?MODULE, {Coords, Range}),
    {Pid, test_entity()}.

get(Pid, Coords) ->
    gen_event:call(Pid, ?MODULE, {get, Coords}).

in_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    [E|_] = get(Pid, {2, 2}).

out_of_range_test() ->
    {Pid, E} = test_init({1,1}, 1),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {5, 5}, nil}),
    [] = get(Pid, {5, 5}).

leaving_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {2,2}, nil}),
    gen_event:sync_notify(Pid, {entity_left_map, E, nil}),
    [] = get(Pid, {2, 2}).

moving_in_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    gen_event:sync_notify(Pid, {entity_moved_on_map, E, {2, 2}, {3, 3}, nil}),
    [] = get(Pid, {2, 2}),
    [E|_] = get(Pid, {3, 3}).

moving_into_the_range_test() ->
    {Pid, E} = test_init({1, 1}, 2),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {5, 5}, nil}),
    gen_event:sync_notify(Pid, {entity_moved_on_map, E, {5, 5}, {2, 2}, nil}),
    [] = get(Pid, {5, 5}),
    [E|_] = get(Pid, {2, 2}).

moving_out_of_the_range_test() ->
    {Pid, E} = test_init({1, 1}, 2),
    gen_event:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    gen_event:sync_notify(Pid, {entity_moved_on_map, E, {2, 2}, {3, 3}, nil}),
    [] = get(Pid, {2, 2}),
    [] = get(Pid, {3, 3}).

-endif.
