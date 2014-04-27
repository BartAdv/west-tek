-module(entity_mgr).
-behaviour(supervisor).

-export([start_link/0, add/4, get/1, remove/1]).
-export([init/1]).

add(Id, Module, Func, Args) ->
    gproc:reg({n, l, Id}, ignored),
    supervisor:start_child(entity_mgr, {Id, {Module, Func, Args}, 
					permanent, 
					1000,
					worker,
					dynamic}).

get(Id) ->
    gproc:where({n, l, Id}).

remove(Id) ->
    supervisor:delete_child(entity_mgr, Id),
    gproc:unreg({n, l, Id}).

start_link() ->
    supervisor:start_link({local, entity_mgr}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 10, 60}, []}}.
