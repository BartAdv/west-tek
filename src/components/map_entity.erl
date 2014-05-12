-module(map_entity).
-behaviour(gen_event).

%% entity behaviour that tracks the map of entity and tries to enter it
%% as soon as possible

-export([init/1, handle_event/2]).

init(_Args) ->
    %% load info from some storage, or require it to be passed as Args?
    {ok, #{}}.

%% tick is not designed/implemented yet
handle_event(tick, {Self, MapId, MapX, MapY}=Data) ->
    case MapId of
	none ->
	    Map = map_mgr:get(MapId),
	    act:enter_map(Self, Map, {MapX, MapY}),
	    {ok, Data}
    end;

handle_event({entity_entered_map, Self, {X, Y}, Map}, {Self, _, _, _}) ->
    MapId = map:get_id(Map),
    {ok, {Self, MapId, X, Y}}.
