-module(map_entity).
-behaviour(gen_event).

%% entity behaviour that tracks the map of entity and tries to enter it
%% as soon as possible

-export([init/1, handle_event/2]).

init({Self, {MapId, _MapX, _MapY}=Origin}) ->
    Map = case map_mgr:get(MapId) of undefined -> none; Pid -> Pid end,
    {ok, {Self, Map, Origin}}. 

%% tick is not designed/implemented yet
handle_event(tick, {Self, CurrMap, {MapId, MapX, MapY}}=Data) ->
    case CurrMap of
	none ->
	    Map = map_mgr:get(MapId),
	    act:enter_map(Self, Map, {MapX, MapY}),
	    {ok, Data};
	_ ->
	    {ok, Data}
    end;

handle_event({entity_entered_map, Self, _Coords, Map}
	    , {Self, _OldMap, Origin}) ->
    {ok, {Self, Map, Origin}};

handle_event({entity_left_map, Self, Map}
	    , {Self, Map, {_MapId, MapX, MapY}=Origin}) ->
    %% attempt return to the map
    act:enter_map(Self, Map, {MapX, MapY}),
    %% and for now just track what happened
    {ok, {Self, none, Origin}};

handle_event(_, State) ->
    {ok, State}.

