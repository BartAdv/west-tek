-module(dev).

-export([init/0
	, spawn_protomap/0
	, restart_entity_mgr/0
	, restart/0
	, entities/0
	, components/1
	, map_entities/1]).

init() ->
    application:start(gproc),
    protomap_mgr:start(),
    map_mgr:start(),
    entity_mgr:start().

restart_entity_mgr() ->
    Pid = whereis(entity_mgr),
    case Pid of
	undefined -> true;
	_ -> unregister(entity_mgr),
	     exit(Pid, kill)
    end,
    entity_mgr:start().

restart() ->
    application:stop(westtek),
    application:start(westtek).

spawn_protomap() ->
    {ok, ProtoMap} = protomap:start("resources/den.fomap"),
    ProtoMap.

entities() ->
    Children = supervisor:which_children(entity_mgr),
    lists:map(fun({_Id,Pid,_,_}) -> Pid end, Children).

map_entities(MapId) ->
    Map = map_mgr:get(MapId),
    #{entities := Es} = map:get_info(Map),
    Es.

components(Pid) ->
    {entity, _, EventMgr} = sys:get_state(Pid),
    sys:get_state(EventMgr).
