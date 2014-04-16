-module(protomap).
-behaviour(gen_server).

-export([init/1]).

-record(proto_map, {header
                   ,hexes}).

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
                        ,fun([_, P, V]) -> {get_atom(Line, P), get_integer(Line, V)}
                         end}
                       ,{"([A-Za-z0-9]+)[\\s\\t]+\"([^\"]+)\""
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
    R = fun R(File) ->
                {ok, Line} = file:read_line(File),
                case re:run(Line, Re) of
                    {match, [_, T, X, Y, S]} ->
                        Type = get_atom(Line, T),
                        Coords = {get_integer(Line, X), get_integer(Line, Y)},
                        Frm = get_str(Line, S),
                        ets:insert(Tab, {Coords, {Type, Frm}}),
                        R(File);
                    nomatch -> Tab
                end
        end,
    R(File).

map_object_type(0) ->
    critter;
map_object_type(1) ->
    item;
map_object_type(2) ->
    scenery.

load_objects(File, Tab) ->
    {ok, Re} = re:compile("MapObjType[\\s\\t]+([0-9]+)"),
    R = fun R(File) ->
                case file:read_line(File) of
                    {ok, Line} ->
                        case re:run(Line, Re) of
                            {match, [_, T]} ->
                                Type = get_integer(Line, T),
                                Obj = load_properties(File, #{type=>map_object_type(get_integer(Line, T))}),
                                #{x:=MapX, x:=MapY} = Obj,
                                ets:insert(Tab, {{MapX, MapY}, Obj}), %% transform type to atom and store it as part of the key
                                R(File);
                            nomatch -> Tab
                        end;
                    eof -> Tab
                end
        end,
    R(File).

load_protomap(File, #proto_map{hexes=Hexes}=ProtoMap) ->
    case file:read_line(File) of
        {ok, "[Header]\n"} ->
            Header = load_properties(File, #{}),
            load_protomap(File, ProtoMap#proto_map{header=Header});
        {ok, "[Tiles]\n"} ->
            Tiles = load_tiles(File, Hexes),
            load_protomap(File, ProtoMap);
        {ok, "[Objects]\n"} ->
            Objects = load_objects(File, Hexes),
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
    Hexes = ets:new(hexes, [bag]),
    {ok, load_protomap(File, #proto_map{hexes=Hexes})}.
