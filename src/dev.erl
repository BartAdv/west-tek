-module(dev).

-export([init/0, spawn_protomap/0, spawn_map/1, restart_entity_mgr/0]).

init() ->
    application:start(gproc),
    entity_mgr:start().

restart_entity_mgr() ->
    catch exit(whereis(entity_mgr), kill),
    entity_mgr:start().

spawn_protomap() ->
    {ok, ProtoMap} = protomap:start("resources/den.fomap"),
    ProtoMap.

spawn_map(ProtoMap) ->
    {ok, Map} = map:start(ProtoMap),
    Map.


