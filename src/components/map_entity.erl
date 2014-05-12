-module(map_entity).
-behaviour(gen_event).

%% entity behaviour that tracks the map of entity and tries to enter it
%% as soon as possible

-export([init/2, handle_event/2]).

init(#{'MapX' := MapX,'MapY' := MapY}, Self) ->
    {ok, {Self, {none, MapX, MapY}}}.

%% tick is not designed/implemented yet
handle_event(tick, {Self, Map, MapX, MapY}=Data) ->
    case Map of
	none ->
	    act:enter_map(Self, Map, {MapX, MapY}),
	    {ok, Data};
	_ ->
	    {ok, Data}
    end;

handle_event({entity_entered_map, Self, {X, Y}, Map}
	    , {Self, _, _, _}) ->
    {ok, {Self, Map, X, Y}};

handle_event({entity_left_map, Self, Map}
	    , {Self, _Map, MapX, MapY}) ->
    %% attempt return to the map
    act:enter_map(Self, Map, {MapX, MapY}),
    %% and for now just track what happened
    {ok, {Self, none, MapX, MapY}}.
