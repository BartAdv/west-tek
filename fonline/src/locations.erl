-module(locations).

-export([init/1]).

init(Path) ->
    {ok, File} = file:open(filename:join(Path, "maps/Locations.cfg"), read),
    load(File, #{}).
   
load(File, Locs) -> 
    case file:read_line(File) of
	{ok, Line} ->
	    case Line of
		[$#|_] -> load(File, Locs);
		[$[|_] -> 
		    {ok, [Id], _} = io_lib:fread("[Area ~d]", Line),
		    Area = load_area(File, #{}),
		    load(File, maps:put(Id, Area, Locs));
		_ -> load(File, Locs)
	    end;
	eof -> Locs
    end.

load_area(File, Area) -> 
    case file:read_line(File) of
	{ok, Line} ->
	    case Line of
		"name" ++ Rest ->
		    {ok, [_], Name} = io_lib:fread("~s ", Rest),
		    load_area(File, Area#{name => string:strip(Name, both, $\n)});
		"map_" ++ Rest ->
		    {ok, [Index, MapName, ProtoId], _} = io_lib:fread("~d = ~s~d", Rest),
		    #{maps := Maps} = maps:merge(Area, #{maps => #{}}),
		    load_area(File, Area#{maps => maps:put(Index, #{name => MapName, pid => ProtoId}, Maps)});
		"size" ++ Rest ->
		    [Size] = scanf(" =~d", Rest),
		    load_area(File, Area#{size => Size});
		"entrance" ++ _ ->
		    %% TODO
		    load_area(File, Area);
		"visible" ++ Rest ->
		    [Visible] = scanf(" =~d", Rest),
		    load_area(File, Area#{visible => Visible});
		"auto_garbage" ++ Rest ->
		    [AutoGarbage] = scanf(" =~d", Rest),
		    load_area(File, Area#{auto_garbage => AutoGarbage});
		"autogarbage" ++ Rest ->
		    [A] = scanf(" =~d", Rest),
		    load_area(File, Area#{auto_garbage => A});
		"geck_visible" ++ Rest ->
		    [GeckVisible] = scanf(" =~d", Rest),
		    load_area(File, Area#{geck_visible => GeckVisible});
		"max_players" ++ Rest ->
		    [Max] = scanf(" =~d", Rest),
		    load_area(File, Area#{max_players => Max});
		[$\n] -> Area
	    end;
	eof -> Area
    end.

scanf(Format, Str) ->
    {ok, Res, _} = io_lib:fread(Format, Str),
    Res.
