-module(protomap).
-behaviour(gen_server).

-export([get/4, get_objects/2]).
-export([init/1, start/1, handle_call/3]).

-record(proto_map, {header
                   ,hexes}).

%% API

get(Pid, X, Y, Type) ->
    gen_server:call(Pid, {get, X, Y, Type}).

get_objects(Pid, Type) ->
    gen_server:call(Pid, {get_objects, Type}).

start(FileName) ->
    gen_server:start(?MODULE, FileName, []).

%% internals

get_str(Str, {S,L}) -> string:substr(Str, S+1, L).

get_integer(Str, R) ->
    {I, _} = string:to_integer(get_str(Str, R)),
    I.

get_atom(Str, R) ->
    list_to_atom(get_str(Str, R)).

load_properties(File, Props) ->
    case file:read_line(File) of
        eof -> Props;
        {ok, Line} ->
            %% need to put it outside to avoid needless reevaluation
            Matchers = [{"([A-Za-z0-9]+)[\\s\\t]+(-*[0-9]+)"
                        ,fun([_, P, V]) -> {get_atom(Line, P), get_integer(Line, V)} end}
                       ,{"([A-Za-z0-9]+)[\\s\\t]+(.+)"
                        ,fun([_, P, V]) -> {get_atom(Line, P), get_str(Line, V)} end}],
            case Line of
                "\n" -> Props;
                _ ->
                    Match = fun Match([{R,F}|T]) ->
                                    case re:run(Line, R) of
                                        {match, M} ->
                                            {P,V} = F(M),
                                            load_properties(File, maps:put(P, V, Props));
                                        nomatch ->
                                            case T of
                                                [] -> load_properties(File, Props);
                                                _ -> Match(T)
                                            end
                                    end
                            end,
                    Match(Matchers)
            end
    end.

load_tiles(File, Tab) ->
    {ok, Re} = re:compile("(tile|roof)[\\s\\t]+([0-9]+)[\\s\\t]+([0-9]+)[\\s\\t]+(.+)$"),
    R = fun R() ->
                {ok, Line} = file:read_line(File),
                case re:run(Line, Re) of
                    {match, [_, T, Mx, My, S]} ->
                        Type = get_atom(Line, T),
                        {X, Y} = {get_integer(Line, Mx), get_integer(Line, My)},
                        Frm = get_str(Line, S),
                        ets:insert(Tab, {{X, Y, Type}, Frm}),
                        R();
                    nomatch -> Tab
                end
        end,
    R().

map_object_type(0) ->
    critter;
map_object_type(1) ->
    item;
map_object_type(2) ->
    scenery.

load_objects(File, Tab) ->
    {ok, Re} = re:compile("MapObjType[\\s\\t]+([0-9]+)"),
    R = fun R() ->
                case file:read_line(File) of
                    {ok, Line} ->
                        case re:run(Line, Re) of
                            {match, [_, T]} ->
                                Type = map_object_type(get_integer(Line, T)),
                                Obj = load_properties(File, #{}),
                                #{'MapX':=X, 'MapY':=Y} = Obj,
                                ets:insert(Tab, {{X, Y, Type}, Obj}), 
                                R();
                            nomatch -> Tab
                        end;
                    eof -> Tab
                end
        end,
    R().

load_protomap(File, #proto_map{hexes=Hexes}=ProtoMap) ->
    case file:read_line(File) of
        {ok, "[Header]\n"} ->
            Header = load_properties(File, #{}),
            load_protomap(File, ProtoMap#proto_map{header=Header});
        {ok, "[Tiles]\n"} ->
            Hexes = load_tiles(File, Hexes),
            load_protomap(File, ProtoMap);
        {ok, "[Objects]\n"} ->
            Hexes = load_objects(File, Hexes),
            load_protomap(File, ProtoMap);
        eof ->
            ProtoMap;
        {ok, Line} ->
            io:format("Unable to parse line: ~s", [Line]),
            exit(unrecognized),
            load_protomap(File, ProtoMap)
    end.

init(FileName) ->
    {ok, File} = file:open(FileName, read),
    Hexes = ets:new(hexes, [duplicate_bag]),
    {ok, load_protomap(File, #proto_map{hexes=Hexes})}.

handle_call({get, X, Y, Type}, _From, #proto_map{hexes=Hx}=ProtoMap) ->
    Res = ets:lookup(Hx, {X, Y, Type}),
    Out = lists:map(fun({{_, _, Type}, Obj}) -> Obj end, Res),
    {reply, Out, ProtoMap};

handle_call({get_objects, Type}, _From, #proto_map{hexes=Hx}=ProtoMap) ->
    Res = ets:match(Hx, {{'$1', '$2', Type}, '$3'}),
    Out = lists:map(fun([X, Y, Obj]) -> {{X, Y}, Obj} end, Res),
    {reply, Out, ProtoMap}.
