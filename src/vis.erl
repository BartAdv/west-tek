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
                  ,range
                  ,entity}).

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

to_local({X1, Y1}, {X2, Y2}) ->
    {X2-X1, Y2-Y1}.

%% gen_event

init({Entity, Coords, Range}) ->
    {ok, #vis_data{entity=Entity, pos=Coords, range=Range}}.

%% self entering the map - 'ask' everyone about their location
handle_event({entity_entered_map, Self, Coords, Map},
	     #vis_data{entity=Self}=Vis) ->
    map:notify(Map, {where_are_you, Self}),
    {ok, Vis};

handle_event({entity_entered_map, Ent, Coords, _Map},
             #vis_data{entity=Self, vis_list=VisList, grid=Grid, pos=Pos, range=Range}=Vis) ->
    case dist(Pos, Coords) =< Range of
        true ->
            Vis2 = add_entity(Vis, to_local(Pos, Coords), Ent),
            entity:notify(Self, {entity_spotted, Ent}),
            {ok, Vis2};
        false ->
            {ok, Vis}
    end;

handle_event({entity_left_map, Ent, _Map},
             #vis_data{entity=Self, vis_list=VisList, grid=Grid}=Vis) ->
    case gb_trees:lookup(Ent, VisList) of
        {value, Coords} ->
            Vis2 = remove_entity(Vis, Coords, Ent),
            entity:notify(Self, {entity_out_of_sight, Ent}),
            {ok, Vis2};
        none -> {ok, Vis}
    end;

handle_event({entity_moved_on_map, Ent, From, To, _Map},
             #vis_data{entity=Self, vis_list=VisList, grid=Grid, pos=Pos, range=Range}=Vis) ->
    LocalFrom = to_local(Pos, From),
    LocalTo = to_local(Pos, To),
    case gb_trees:lookup(Ent, VisList) of
        {value, Coords} ->
            case dist(Pos, To) =< Range of
                true ->
                    {ok, move_entity(Vis, LocalFrom, LocalTo, Ent)};
                false ->
                    Vis2 = remove_entity(Vis, LocalFrom, Ent),
                    entity:notify(Self, {entity_out_of_sight, Ent}),
                    {ok, Vis2}
            end;
        none ->
            case dist(Pos, To) =< Range of
                true ->
                    Vis2 = add_entity(Vis, LocalTo, Ent),
                    entity:notify(Self, {entity_spotted, Ent}),
                    {ok, Vis2};
                false ->
                    {ok, Vis}
            end
    end;

handle_event({where_are_you, Asking}, #vis_data{entity=Self, pos=Pos}=Vis) ->
    entity:notify(Asking, {i_am_here, Self, Pos}),
    {ok, Vis};

handle_event({i_am_here, Self, _}, #vis_data{entity=Self}=Vis) ->
    %% don't do anything for itself
    {ok, Vis};

handle_event({i_am_here, Who, Coords}, 
	     #vis_data{pos=Pos, range=Range}=Vis) ->
    case dist(Pos, Coords) =< Range of
	true ->
	    Vis2 = add_entity(Vis, to_local(Pos, Coords), Who),
	    {ok, Vis2};
	false ->
	    {ok, Vis}
    end;

handle_event(_, Vis) ->
    {ok, Vis}.


handle_call({set_range, Range}, Vis) ->
    {ok, ok, Vis#vis_data{range=Range}};

handle_call(get, #vis_data{vis_list=VisList}=Vis) ->
    {ok, gb_trees:to_list(VisList), Vis};

handle_call({get, Coords}, #vis_data{grid=Grid}=Vis) ->
    Hex = get_hex(Grid, Coords),
    {ok, sets:to_list(Hex), Vis}.

-ifdef(TEST).

test_init(Coords, Range) ->
    {ok, Self} = entity:start_link(),
    entity:add_handler(Self, test_handler, []),
    entity:add_handler(Self, ?MODULE, {Self, Coords, Range}),
    {ok, Other} = entity:start_link(),
    {Self, Other}.

get_vis_list(Pid) ->
    entity:call(Pid, ?MODULE, get).

get(Pid, Coords) ->
    entity:call(Pid, ?MODULE, {get, Coords}).

in_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    entity:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    [E|_] = get(Pid, {1, 1}).

out_of_range_test() ->
    {Pid, E} = test_init({1,1}, 1),
    entity:sync_notify(Pid, {entity_entered_map, E, {5, 5}, nil}),
    [] = get(Pid, {4, 4}).

leaving_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    entity:sync_notify(Pid, {entity_entered_map, E, {2,2}, nil}),
    entity:sync_notify(Pid, {entity_left_map, E, nil}),
    [] = get(Pid, {1, 1}).

moving_in_range_test() ->
    {Pid, E} = test_init({1,1}, 10),
    entity:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    entity:sync_notify(Pid, {entity_moved_on_map, E, {2, 2}, {3, 3}, nil}),
    [] = get(Pid, {1, 1}),
    [E|_] = get(Pid, {2, 2}).

moving_into_the_range_test() ->
    {Pid, E} = test_init({1, 1}, 2),
    entity:sync_notify(Pid, {entity_entered_map, E, {5, 5}, nil}),
    entity:sync_notify(Pid, {entity_moved_on_map, E, {5, 5}, {2, 2}, nil}),
    [] = get(Pid, {4, 4}),
    [E|_] = get(Pid, {1, 1}).

moving_out_of_the_range_test() ->
    {Pid, E} = test_init({1, 1}, 2),
    entity:sync_notify(Pid, {entity_entered_map, E, {2, 2}, nil}),
    entity:sync_notify(Pid, {entity_moved_on_map, E, {2, 2}, {3, 3}, nil}),
    [] = get(Pid, {1, 1}),
    [] = get(Pid, {1, 1}).

entering_map_sends_position_request_test() ->
    {Pid, _} = test_init({1,1}, 2),
    {ok, Map} = map:start_link(),
    map:add_handler(Map, test_handler, []),
    Es = [E || {ok, E} <- [entity:start_link() || _ <- [1,2,3]]],
    lists:foreach(fun(E) -> map:add_entity(Map, E, {2,2}) end, Es),
    entity:sync_notify(Pid, {entity_entered_map, Pid, {1,1}, Map}),
    Evs = entity:call(Map, test_handler, dump),
    Evs = [{where_are_you, Pid}].

position_request_test() ->
    {Pid, E} = test_init({2,3}, 5),
    entity:add_handler(E, test_handler, []),
    entity:sync_notify(Pid, {where_are_you, E}),
    Evs = entity:call(E, test_handler, dump),
    Evs = [{i_am_here, Pid, {2,3}}].

position_request_range_test() ->
    {Pid, E} = test_init({1,1}, 2),
    entity:sync_notify(Pid, {i_am_here, E, {5,5}}),
    [] = get_vis_list(Pid),
    entity:sync_notify(Pid, {i_am_here, E, {2,2}}),
    [{E,{1,1}}] = get_vis_list(Pid).


-endif.
