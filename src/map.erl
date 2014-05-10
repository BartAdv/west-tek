-module(map).
-behaviour(gen_server).

-export([add_entity/2, remove_entity/2, register/2]).
-export([init/1, terminate/2, start/0, start_link/0, start/1, start_link/1, handle_call/3, handle_info/2]).
-export([notify/2]).
-export([get_info/1]).

-define(TEST, 1).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(map_data, {proto
		  ,entities = gb_trees:empty()}).

%% API
add_entity(Pid, Ent) ->
    gen_server:call(Pid, {add_entity, Ent}).

remove_entity(Pid, Ent) ->
    gen_server:call(Pid, {remove_entity, Ent}).

register(Pid, Id) ->
    gen_server:call(Pid, {register, Id}).

add_handler(Pid, Handler, Args) ->
    gen_server:call(Pid, {add_handler, Handler, Args}).

call(Pid, Handler, Request) ->
    gen_server:call(Pid, {call, Handler, Request}).

notify(Pid, Event) ->
    gen_server:call(Pid, {notify, Event}).

get_info(Pid) ->
    gen_server:call(Pid, get_info).

%% internals

iter_entities(Es, F) ->
    Fr = fun Fr(Iter) ->
                 case gb_trees:next(Iter) of
                     {Ent, _, Iter2} -> F(Ent), Fr(Iter2);
                     none -> nil
                 end
         end,
    Fr(gb_trees:iterator(Es)).

map_add_entity(#map_data{entities=Es}=Map, Ent) ->
    case gb_trees:is_defined(Ent, Es) of
        false ->
            Ref = monitor(process, Ent),
            Es2 = gb_trees:insert(Ent, Ref, Es),
            Map#map_data{entities=Es2};
        true -> already_added
end.

map_remove_entity(#map_data{entities=Es}=Map, Ent) ->
    Ref = gb_trees:get(Ent, Es),
    demonitor(Ref),
    Es2 = gb_trees:delete(Ent, Es),
    Map#map_data{entities = Es2}.

%% Server

start() ->
    gen_server:start(?MODULE, null, []).

start(ProtoMap) ->
    gen_server:start(?MODULE, ProtoMap, []).

start_link() ->
    gen_server:start_link(?MODULE, null, []).

start_link(ProtoMap) ->
    gen_server:start_link(?MODULE, ProtoMap, []).

init(null) ->
    {ok, #map_data{}};
init(ProtoMap) ->
    monitor(process, ProtoMap),
    {ok, #map_data{proto=ProtoMap}}.

terminate(_Reason, _) ->
    ok.

handle_call({add_entity, Ent}, _From, Map) ->
    case map_add_entity(Map, Ent) of
        already_added -> {reply, already_added, Map};
        NewMap -> {reply, ok, NewMap}
    end;

handle_call({remove_entity, Ent}, _From, Map) ->
    {reply, ok, map_remove_entity(Map, Ent)};

handle_call({notify, Event}, _From, #map_data{entities=Es}=Map) ->
    iter_entities(Es, fun(Ent) -> entity:notify(Ent, Event) end),
    {reply, ok, Map};

handle_call({register, Id}, _From, Map) ->
    Pid = map_mgr:register(Id),
    {reply, Pid, Map};
    
handle_call(get_info, _From, #map_data{entities=Es}=Map) ->
    {reply, #{entities_count => gb_trees:size(Es)}, Map}.


%% when Entity process dies, we need to remove our references to it
handle_info({'DOWN', Ref, process, Pid, _Reason}, #map_data{entities=Es}=Map) ->
    case gb_trees:lookup(Pid, Es) of
        {value, Ref} ->
            {noreply, map_remove_entity(Map, Pid)}
    end;
handle_info({'DOWN', Ref, process, ProtoMap, _Reason}
	   ,#map_data{proto=ProtoMap}=Map) ->
    {stop, proto, Map#map_data{proto=nil}}.

-ifdef(TEST).

test_entity() ->
   spawn(fun()-> receive kill -> exit(killed) end end).

add_remove_test() ->
    Cr = test_entity(),
    {ok, Pid} = map:start_link(),
    map:add_entity(Pid, Cr),
    map:remove_entity(Pid, Cr),
    ok=map:add_entity(Pid, Cr).

cannot_add_already_added_test() ->
    Cr = test_entity(),
    {ok, Pid} = map:start_link(),
    map:add_entity(Pid, Cr),
    already_added = map:add_entity(Pid, Cr).

-endif.
