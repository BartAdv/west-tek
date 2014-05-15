-module(map_mgr).
-behaviour(supervisor).

-export([start_link/0, add/4, register/1, get/1, remove/1]).
-export([init/1]).

add(Id, Module, Func, Args) ->
    {ok, Pid} = supervisor:start_child(map_mgr, {Id, {Module, Func, Args}, 
						 permanent, 
						 1000,
						 worker,
						 dynamic}),
    {ok, Pid}.

register(Id) ->
    gproc:reg({n, l, {map, Id}}, ignored).

get(Id) ->
    gproc:where({n, l, {map, Id}}).

remove(Id) ->
    supervisor:delete_child(map_mgr, Id),
    gproc:unreg({n, l, {map, Id}}).

start_link() ->
    supervisor:start_link({local, map_mgr}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 10, 60}, []}}.
