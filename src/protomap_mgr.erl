-module(protomap_mgr).
-behaviour(gen_server).

-export([start/0, start_link/0, register/1, get/1, remove/1]).
-export([init/1, handle_call/3, handle_info/2]).

-include_lib("stdlib/include/qlc.hrl").

get(ProtoName) ->
    %% this one call is using gen_server to avoid concurrent loads
    %% of same resource
    gen_server:call(protomap_mgr, {get, ProtoName}).

register(ProtoName) ->
    gproc:reg({n, l, {protomap, ProtoName}}, ignored).

remove(ProtoName) ->
    gproc:unreg({n, l, {protomap, ProtoName}}).

start() ->
    gen_server:start({local, protomap_mgr}, ?MODULE, [], []).
   
start_link() ->
    gen_server:start_link({local, protomap_mgr}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

load(ProtoName) ->
    {ok, Pid} = protomap:start_link(ProtoName),
    Pid.

handle_call({get, ProtoName}, _From, State) ->
    ProtoMap = case gproc:where({n, l, {protomap, ProtoName}}) of
		   undefined ->
		       load(ProtoName);
		   Pid -> Pid
	       end,
    {reply, ProtoMap, State}.

reverse_lookup(Pid) ->
    Q = qlc:q([ProtoName || {{n, l, {protomap, ProtoName}}, P, _} <- gproc:table(props), P == Pid]),
    qlc:e(Q).

handle_info({'EXIT', From, Reason}, State) ->
    case reverse_lookup(From) of
	[ProtoName] -> 
	    load(ProtoName);
	_ -> exit(self(), Reason)
    end,
    {noreply, State}.
