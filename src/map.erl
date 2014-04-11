-module(map).
-behaviour(gen_server).

-export([add_critter/2, remove_critter/2]).
-export([init/1, terminate/2, start_link/0, handle_call/3, handle_info/2]).
-export([add_handler/3, call/3, notify/2]).

-export([test/0]).

-record(map_data, {critters=gb_trees:empty(),
                   event_mgr}).

%% API
add_critter(Pid, Cr) ->
    gen_server:call(Pid, {add_critter, Cr}).

remove_critter(Pid, Cr) ->
    gen_server:call(Pid, {remove_critter, Cr}).

add_handler(Pid, Handler, Args) ->
    gen_server:call(Pid, {add_handler, Handler, Args}).

call(Pid, Handler, Request) ->
    gen_server:call(Pid, {call, Handler, Request}).

notify(Pid, Event) ->
    gen_server:call(Pid, {notify, Event}).

%% internals

map_add_critter(#map_data{critters=Crits}=Map, Cr) ->
    Ref = monitor(process, Cr),
    Map#map_data{critters=gb_trees:insert(Cr, {critter, Ref}, Crits)}.

map_remove_critter(#map_data{critters=Crits}=Map, Cr) ->
    {critter, Ref} = gb_trees:get(Cr, Crits),
    demonitor(Ref),
    Map#map_data{critters = gb_trees:delete(Cr, Crits)}.

%% Server

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, EventMgr} = gen_event:start_link(),
    {ok, #map_data{event_mgr=EventMgr}}.

terminate(_Reason, _) ->
    ok.

handle_call({add_critter, Cr}, _From, Map) ->
    {reply, ok, map_add_critter(Map, Cr)};

handle_call({remove_critter, Cr}, _From, Map) ->
    {reply, ok, map_remove_critter(Map, Cr)};

handle_call({notify, Event}, _From, #map_data{event_mgr=EventMgr}=Map) ->
    gen_event:notify(EventMgr, Event),
    {reply, ok, Map};

handle_call({call, Handler, Request}, _From, #map_data{event_mgr=EventMgr}=Map) ->
    Res = gen_event:call(EventMgr, Handler, Request),
    {reply, Res, Map};

handle_call({add_handler, Handler, Args}, _From, #map_data{event_mgr=EventMgr}=Map) ->
    Res = gen_event:add_handler(EventMgr, Handler, Args),
    {reply, Res, Map}.


%% when Critter process dies, we need to remove our references to it
handle_info({'DOWN', Ref, process, Pid, _Reason}, #map_data{critters=Crits}=Map) ->
    case gb_trees:lookup(Pid, Crits) of
        {value, {critter, Ref}} ->
            {noreply, map_remove_critter(Map, Pid)}
        end.

test() ->
    Cr = spawn(fun() -> receive kill -> exit(ok) end end),
    {ok, Pid} = map:start_link(),
    map:add_critter(Pid, Cr),
    map:remove_critter(Pid, Cr),
    map:add_critter(Pid, Cr),
    Cr ! kill.
