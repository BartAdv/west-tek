-module(map_mgr).
-behaviour(supervisor).

-export([start/0, start_link/0, add/2, register/1, get/1, remove/1]).
-export([init/1, start_map/2]).

add(Id, ProtoId) ->
    {ok, Pid} = supervisor:start_child(map_mgr, {Id, {map_mgr, start_map, [Id, ProtoId]}, 
						 transient, 
						 1000,
						 worker,
						 dynamic}),
    {ok, Pid}.

start_map(Id, ProtoId) ->
    case protomap_mgr:get(ProtoId) of
	{ok, ProtoMap} -> 
	    io:format("Spawning map {~w, \"~s\"}~n", [Id, ProtoId]),
	    map:start_link(Id, ProtoMap);
	{error, Reason} -> 
	    io:format("Unable to spawn map: ~w~n", [Reason]),
	    ignore
    end.

register(Id) ->
    gproc:reg({n, l, {map, Id}}, ignored).

get(Id) ->
    gproc:where({n, l, {map, Id}}).

remove(Id) ->
    supervisor:delete_child(map_mgr, Id),
    gproc:unreg({n, l, {map, Id}}).

start_link() ->
    supervisor:start_link({local, map_mgr}, ?MODULE, []).
   
start() ->
    {ok, Pid} = start_link(), 
    unlink(Pid),
    {ok, Pid}.
   
init(_Args) ->
    {ok, {{one_for_one, 10, 60}, []}}.
