-module(map).
-behaviour(gen_server).

-export([add_critter/4, remove_critter/2]).
-export([init/1, terminate/2, start_link/0, handle_call/3, handle_info/2]).

-record(hex, {critters=sets:new(), items=sets:new()}).
-record(map_data, {critters=gb_trees:empty(),
                   grid=gb_trees:empty()}).

%% API
add_critter(Pid, Cr, HexX, HexY) ->
    gen_server:call(Pid, {add_critter, Cr, HexX, HexY}).

remove_critter(Pid, Cr) ->
    gen_server:call(Pid, {remove_critter, Cr}).

%% internals

%% lookup hex given the coordinates
%% this always returns valid hex
get_hex(#map_data{grid=Grid}, Coords) ->
    case gb_trees:lookup(Coords, Grid) of
        none -> #hex{};
        {value, H} -> H
    end.

map_remove_critter(#map_data{critters=Crits, grid=Grid}=Map, Cr) ->
    case gb_trees:lookup(Cr, Crits) of
        {value, Coords} ->
            Hex = #hex{critters=HexCrits} = get_hex(Map, Coords),
            Map#map_data{critters=gb_trees:delete(Cr, Crits),
                         grid=gb_trees:enter(Coords, Hex#hex{critters=sets:del_element(Cr, HexCrits)}, Grid)};
        none -> Map
    end.

%% Server

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #map_data{}}.

terminate(_Reason, _) ->
    ok.

handle_call({add_critter, Cr, HexX, HexY}, _From, #map_data{critters=Crits, grid=Grid}=Map) ->
    Coords = {HexX, HexY},
    #hex{critters=HexCrits} = Hex = get_hex(Map, Coords),
    NewHex = Hex#hex{critters=sets:add_element(Cr, HexCrits)},
    monitor(process, Cr),
    {reply, ok, Map#map_data{critters=gb_trees:insert(Cr, Coords, Crits),
                             grid=gb_trees:enter(Coords, NewHex, Grid)}};

handle_call({remove_critter, Cr}, _From, Map) ->
    {reply, ok, map_remove_critter(Map, Cr)}.

%% when Critter process dies, we need to remove our references to it
handle_info({'DOWN', _Ref, process, Cr, _Reason}, Map) ->
    {noreply, map_remove_critter(Map, Cr)}.
