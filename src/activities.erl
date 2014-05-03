-module(activities).

-export([enter_map/3, exit_map/2]).

-define(TEST, 1).

-ifdef(TEST).
-export([test_entity/0, test_map/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

enter_map(EntityId, MapId, Coords) ->
    Entity = entity_mgr:get(EntityId),
    Map = map_mgr:get(MapId),
    map:add_entity(Map, Entity),
    map:notify_entities(Map, {entity_entered_map, Entity, Coords, Map}).

exit_map(EntityId, MapId) ->
    Entity = entity_mgr:get(EntityId),
    Map = map_mgr:get(MapId),
    map:remove_entity(Map, Entity),
    map:notify_entities(Map, {entity_left_map, Entity, Map}).

-ifdef(TEST).

test_entity() ->
    entity:start().

test_map() ->
    map:start().

i_dont_yet_know_how_to_make_eunit_fixtures_test() ->
    application:start(gproc).

test_init() ->
    gproc:goodbye(),
    entity_mgr:start_link(),
    map_mgr:start_link(),
    entity_mgr:add(1, activities, test_entity, []),
    map_mgr:add(1, activities, test_map, []).

enter_map_test() ->
    test_init(),
    enter_map(1, 1, {2,2}),
    #{entities_count:=1} = map:get_info(map_mgr:get(1)).

-endif.
