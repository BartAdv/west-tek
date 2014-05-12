-module(critter).

%% functions that create critter entities based on the Proto data

-export([start/1]).

start(Proto) ->
    {ok, Pid} = entity:start_link(),
    #{'MapX' := MapX ,'MapY' := MapY} = Proto,
    entity:add_handler(Pid, vis, {Pid, {MapX, MapY}, 10}),
    entity:add_handler(Pid, map_npc, {none, MapX, MapY}),
    {ok, Pid}.
