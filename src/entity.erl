-module(entity).
-behaviour(gen_server).

-export([register/2, notify/2, sync_notify/2, add_handler/3, call/3]).
-export([start/0, start/1, start_link/0, start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-record(entity, {event_mgr}).

%% API

register(Pid, Id) ->
    gen_server:call(Pid, {register, Id}).

notify(Pid, Event) ->
    gen_server:cast(Pid, {notify, Event}).

sync_notify(Pid, Event) ->
    gen_server:call(Pid, {sync_notify, Event}).

call(Pid, Handler, Request) ->
    gen_server:call(Pid, {call, Handler, Request}).

add_handler(Pid, Handler, Args) ->
    gen_server:call(Pid, {add_handler, Handler, Args}).

init(Proto) ->
    {ok, EventMgr} = gen_event:start_link(),
    {ok, #entity{event_mgr=EventMgr}}.

terminate(_Reason, _) ->
    ok.

start_link() ->
    gen_server:start_link(?MODULE, #{}, []).

start_link(Proto) ->
    gen_server:start_link(?MODULE, Proto, []).

start() ->
    gen_server:start(?MODULE, #{}, []).

start(Proto) ->
    gen_server:start(?MODULE, Proto, []).

handle_call({sync_notify, Event}, _From, #entity{event_mgr=EventMgr}=E) ->
    {reply, gen_event:sync_notify(EventMgr, Event), E};

handle_call({add_handler, Handler, Args},
            _From,
            #entity{event_mgr=EventMgr}=E) ->
    %% add_sup_handler so that when component fails, we will know
    gen_event:add_sup_handler(EventMgr, Handler, Args),
    {reply, ok, E};

handle_call({call, Handler, Request}, _From, #entity{event_mgr=EventMgr}=E) ->
    {reply, gen_event:call(EventMgr, Handler, Request), E};

handle_call({register, Id}, _From, E) ->
    {reply, entity_mgr:register(Id), E}.

handle_cast({notify, Event}, #entity{event_mgr=EventMgr}=E) ->
    gen_event:notify(EventMgr, Event),
    {noreply, E}.

handle_info({gen_event_EXIT, Module, Reason}, E) ->
    {stop, {handler_crashed, Module, Reason}, E}.
