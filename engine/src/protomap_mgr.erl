-module(protomap_mgr).
-behaviour(gen_server).

-export([start/0, start_link/0, register/1, get/1, remove/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

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
    {ok, []}.

load(ProtoName) ->
    case protomap:start(ProtoName) of
	{ok, Pid} ->
	    monitor(process, Pid),
	    {ok, Pid};
	{error, Reason} ->
	    {error, Reason}
    end.

handle_call({get, ProtoName}, _From, State) ->
    ProtoMap = case gproc:where({n, l, {protomap, ProtoName}}) of
		   undefined ->
		       load(ProtoName);
		   Pid -> {ok, Pid}
	       end,
    {reply, ProtoMap, State}.

handle_info({'DOWN', _Ref, process, _Pid, Info}, State) ->
    io:format("ProtoMap died: ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
