-module(critter).

%% functions that create critter entities based on the Proto data

-export([start/1, start_link/1]).

init(Data, Pid) ->
    #{proto := Proto} = Data,
    #{'MapX' := MapX ,'MapY' := MapY} = Proto,
    entity:add_handler(Pid, vis, {Pid, {MapX, MapY}, 10}),
    entity:add_handler(Pid, map_entity, {none, MapX, MapY}).

start(Data) ->
    {ok, Pid} = entity:start(),
    init(Data, Pid),
    {ok, Pid}.

start_link(Data) ->
    {ok, Pid} = entity:start_link(),
    init(Data, Pid),
    {ok, Pid}.
