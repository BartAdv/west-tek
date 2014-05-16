-module(map_mgr).
-behaviour(supervisor).

-export([start/0, start_link/0, add/2, register/1, get/1, remove/1]).
-export([init/1, start_map/2]).

add(Id, ProtoId) ->
    {ok, Pid} = supervisor:start_child(map_mgr, {Id, {map_mgr, start_map, [Id, ProtoId]}, 
						 permanent, 
						 1000,
						 worker,
						 dynamic}),
    {ok, Pid}.

start_map(Id, ProtoId) ->
    ProtoMap = protomap_mgr:get(ProtoId),
    map:start_link(Id, ProtoMap).

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
