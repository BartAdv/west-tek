-module(act).

-export([enter_map/3, exit_map/2]).
 
-define(TEST, 1).
 
-ifdef(TEST).
-export([test_entity/0, test_map/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

enter_map(Entity, Map, Coords) ->
    map:add_entity(Map, Entity),
    map:notify(Map, {entity_entered_map, Entity, Coords, Map}).

exit_map(Entity, Map) ->
    map:remove_entity(Map, Entity),
    map:notify(Map, {entity_left_map, Entity, Map}).

-ifdef(TEST).

test_entity() ->
    {ok, Pid} = entity:start(),
    Pid.

test_map() ->
    {ok, Pid} = map:start(),
    Pid.

enter_map_test() ->
    Map = test_map(),
    enter_map(test_entity(), Map, {2,2}),
    #{entities_count:=1} = map:get_info(Map).

-endif.
