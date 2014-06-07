-module(proto_mgr).
-behaviour(gen_server).

-export([start/0, get/2]).
-export([init/1, handle_call/3]).

-include("properties.hrl").

get(Type, ProtoId) ->
    [{_, Proto}] = gen_server:call(proto_mgr, {get, Type, ProtoId}),
    Proto.

start() ->
    gen_server:start({local, proto_mgr}, ?MODULE, [], []).

get_prop_matchers() ->
    lists:map(fun({Re, M}) -> {ok, C} = re:compile(Re), {C, M} end
	     ,[{"([A-Za-z0-9]+)=(-*[0-9]+)"
	       ,fun(L, [_, P, V]) -> {get_atom(L, P), get_integer(L, V)} end}
	      ,{"([A-Za-z0-9]+)=(.+)"
	       ,fun(L, [_, P, V]) -> {get_atom(L, P), get_str(L, V)} end}]).

load_protos(File, Tab) ->
    Matchers = get_prop_matchers(),
    case file:read_line(File) of
	{ok, "[Critter proto]\n"} ->
	    Proto = load_properties(File, Matchers, #{}),
	    #{'Pid' := Pid} = Proto,
	    ets:insert(Tab, {{critter, Pid}, Proto}),
	    load_protos(File, Tab);
	{ok, "[Proto]\n"} ->
	    Proto = load_properties(File, Matchers, #{}),
	    #{'ProtoId' := Pid} = Proto,
	    ets:insert(Tab, {{item, Pid}, Proto}),
	    load_protos(File, Tab);
	{ok, [$#|_]} ->
	    load_protos(File, Tab);
	{ok, [$\n]} ->
	    load_protos(File, Tab);
	eof ->
	    ok
    end.
    
load_lst(File, Tab) ->
    case file:read_line(File) of
	{ok, Line} ->
	    FileName = string:strip(Line, right, $\n),
	    {ok, ProtoFile} = file:open(filename:join("resources/proto", FileName), read),
	    ok = load_protos(ProtoFile, Tab),
	    load_lst(File, Tab);
	eof ->
	    ok
    end.

init(_Args) ->
    {ok, File} = file:open("resources/proto/proto.lst", read),
    Tab = ets:new(proto, [bag]),
    ok = load_lst(File, Tab),
    {ok, Tab}.

handle_call({get, Type, ProtoId}, _From, Tab) ->
    Res = ets:lookup(Tab, {Type, ProtoId}),
    {reply, Res, Tab}.

