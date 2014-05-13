-module(critter).

%% functions that create critter entities based on the Proto data

-export([start/1, start_link/1]).

init(Proto, Pid) ->
    #{'MapX' := MapX ,'MapY' := MapY} = Proto,
    entity:add_handler(Pid, vis, {Pid, {MapX, MapY}, 10}),
    entity:add_handler(Pid, map_entity, {none, MapX, MapY}).

start(Proto) ->
    {ok, Pid} = entity:start(),
    init(Proto, Pid),
    {ok, Pid}.

start_link(Proto) ->
    {ok, Pid} = entity:start_link(),
    init(Proto, Pid),
    {ok, Pid}.
