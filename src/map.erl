-module(map).
-behaviour(gen_server).

-export([add_entity/2, remove_entity/2]).
-export([init/1, terminate/2, start/0, start_link/0, start/2, start_link/2]).
-export([handle_call/3, handle_info/2]).
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
    Ref = monitor(process, Ent),
    Es2 = gb_trees:insert(Ent, Ref, Es),
    Map#map_data{entities=Es2}.


map_remove_entity(#map_data{entities=Es}=Map, Ent) ->
    Ref = gb_trees:get(Ent, Es),
    demonitor(Ref),
    Es2 = gb_trees:delete(Ent, Es),
    Map#map_data{entities = Es2}.

scenery(_Proto) ->
    {ok, Pid} = entity:start_link(),
    %% add scenery handler with Proto data
    Pid.

map_spawn_scenery(ProtoMap, Map) ->
    Scenery = lists:filtermap(fun({_Coords, Proto}) -> 
				      case maps:find('CanUse', Proto) of
					  {ok, 1} -> {true, Proto};
					  {ok, _} -> false;
					  error -> false  
				      end
			      end
			     , protomap:get_objects(ProtoMap, scenery)),
    lists:foldl(fun(Proto, Curr) -> 
			map_add_entity(Curr, scenery(Proto))
		end, Map, Scenery).

map_spawn_entities(ProtoMap, MapId, Map) ->
    Entities = lists:map(fun({_Coords, Proto}) -> Proto end, protomap:get_objects(ProtoMap, critter)),
    lists:foldl(fun(Proto, Curr) ->
			{ok, E} = entity_mgr:add(critter, start_link, 
						 #{proto => Proto
						  ,'MapId' => MapId}),
			map_add_entity(Curr, E)
		end, Map, Entities).

%% Server

start() ->
    gen_server:start(?MODULE, null, []).

start(Id, ProtoName) ->
    gen_server:start(?MODULE, {Id, ProtoName}, []).

start_link() ->
    gen_server:start_link(?MODULE, null, []).

start_link(Id, ProtoName) ->
    gen_server:start_link(?MODULE, {Id, ProtoName}, []).

init(null) ->
    {ok, #map_data{}};
init({Id, ProtoName}) ->
    ProtoMap = protomap_mgr:get(ProtoName),

    Init = fn:comp(fn:partial(fun map_spawn_scenery/2, ProtoMap)
		  ,fn:partial(fun map_spawn_entities/3, ProtoMap, Id)),
    Map  = Init(#map_data{proto = ProtoMap}),
    
    map_mgr:register(Id),
    monitor(process, ProtoMap),
    {ok, Map}.
 
terminate(_Reason, _) ->
    ok.

handle_call({add_entity, Ent}, _From, Map) ->
    {reply, ok, map_add_entity(Map, Ent)};

handle_call({remove_entity, Ent}, _From, Map) ->
    {reply, ok, map_remove_entity(Map, Ent)};

handle_call({notify, Event}, _From, #map_data{entities=Es}=Map) ->
    iter_entities(Es, fun(Ent) -> entity:notify(Ent, Event) end),
    {reply, ok, Map};

handle_call(get_info, _From, #map_data{entities=Es}=Map) ->
    {reply, #{entities_count => gb_trees:size(Es)
	     ,entities => lists:map(fun({E,_}) -> E end, gb_trees:to_list(Es)) }, Map}.

handle_info({'DOWN', _Ref, process, ProtoMap, _Reason}
	   ,#map_data{proto=ProtoMap}=Map) ->
    {stop, proto, Map#map_data{proto=nil}};

%% when Entity process dies, we need to remove our references to it
handle_info({'DOWN', Ref, process, Pid, _Reason}
	   , #map_data{entities = Es}=Map) ->
    case gb_trees:lookup(Pid, Es) of
        {value, Ref} ->
            {noreply, map_remove_entity(Map, Pid)}
    end.

-ifdef(TEST).

test_entity() ->
   spawn(fun()-> receive kill -> exit(killed) end end).

add_remove_test() ->
    Cr = test_entity(),
    {ok, Pid} = map:start_link(),
    map:add_entity(Pid, Cr),
    map:remove_entity(Pid, Cr),
    ok=map:add_entity(Pid, Cr).

-endif.
