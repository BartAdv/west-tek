get_str(Str, {S,L}) -> string:substr(Str, S+1, L).

get_integer(Str, R) ->
    {I, _} = string:to_integer(get_str(Str, R)),
    I.

get_atom(Str, R) ->
    list_to_atom(get_str(Str, R)).

load_properties(File, Matchers, Props) ->
    case file:read_line(File) of
	eof -> Props;
	{ok, Line} ->
	    case Line of
		"\n" -> Props;
		_ ->
		    Match = fun Match([{R,F}|T]) ->
				    case re:run(Line, R) of
					{match, M} ->
					    {P,V} = F(Line, M),
					    load_properties(File, Matchers, maps:put(P, V, Props));
					nomatch ->
					    case T of
						[] -> load_properties(File, Matchers, Props);
						_ -> Match(T)
					    end
				    end
			    end,
		    Match(Matchers)
	    end
    end.
