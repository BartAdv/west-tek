-module(critter).

-behaviour(gen_server).

-export([start/0]).

start() ->
    {ok, Pid} = entity:start(),
    entity:add_handler(Pid, vis, {Pid, {0,0}, 10}),
    Pid.
