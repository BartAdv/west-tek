-module(map).
-behaviour(gen_server).

-export([add_critter/4, remove_critter/2]).
-export([init/1, terminate/2, start_link/0, handle_call/3]).

-record(hex, {critters=sets:new(), items=sets:new()}).
-record(map_data, {critters=gb_trees:empty(),
                  grid=gb_trees:empty()}).

%% API
add_critter(Pid, Id, HexX, HexY) ->
    gen_server:call(Pid, {add_critter, Id, HexX, HexY}).

remove_critter(Pid, Id) ->
    gen_server:call(Pid, {remove_critter, Id}).

%% internals

%% lookup hex given the coordinates
%% this always returns valid hex
get_hex(#map_data{grid=Grid}, {HexX, HexY}=Coords) ->
    case gb_trees:lookup(Coords, Grid) of
        none -> #hex{};
        {value, H} -> H
    end.

%% Server

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #map_data{}}.

terminate(Reason, _) ->
    ok.

handle_call({add_critter, Id, HexX, HexY}, _From, #map_data{critters=Crits, grid=Grid}=Map) ->
    Coords = {HexX, HexY},
    #hex{critters=HexCrits} = Hex = get_hex(Map, Coords),
    case sets:size(HexCrits) of
       0 ->
            NewHex = Hex#hex{critters=sets:add_element(Id, HexCrits)},
            {reply, ok, Map#map_data{critters=gb_trees:insert(Id, Coords, Crits),
                                     grid=gb_trees:enter(Coords, NewHex, Grid)}};
       _ -> {reply, hex_occupied, Map}
    end;

handle_call({remove_critter, Id}, _From, #map_data{critters=Crits, grid=Grid}=Map) ->
    case gb_trees:lookup(Id, Crits) of
        {value, Coords} ->
            Hex = #hex{critters=HexCrits} = get_hex(Map, Coords),
            {reply, ok, Map#map_data{critters=gb_trees:delete(Id, Crits),
                                     grid=gb_trees:enter(Coords, Hex#hex{critters=sets:del_element(Id, HexCrits)}, Grid)}};
        none -> {reply, ok, Map}
    end.
