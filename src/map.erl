-module(map).
-behaviour(gen_server).

-export([add_critter/2, remove_critter/2]).
-export([init/1, terminate/2, start_link/0, handle_call/3, handle_info/2]).

-record(map_data, {critters=sets:new()}).

%% API
add_critter(Pid, Cr) ->
    gen_server:call(Pid, {add_critter, Cr}).

remove_critter(Pid, Cr) ->
    gen_server:call(Pid, {remove_critter, Cr}).

%% internals

map_remove_critter(#map_data{critters=Crits}=Map, Cr) ->
    Map#map_data{critters = sets:del_element(Cr, Crits)}.

%% Server

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #map_data{}}.

terminate(_Reason, _) ->
    ok.

handle_call({add_critter, Cr}, _From, #map_data{critters=Crits}=Map) ->
    monitor(process, Cr),
    {reply, ok, Map#map_data{critters=sets:add_element(Cr, Crits)}};

handle_call({remove_critter, Cr}, _From, Map) ->
    {reply, ok, map_remove_critter(Map, Cr)}.

%% when Critter process dies, we need to remove our references to it
handle_info({'DOWN', _Ref, process, Cr, _Reason}, Map) ->
    {noreply, map_remove_critter(Map, Cr)}.
