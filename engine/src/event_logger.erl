-module(event_logger).
-behaviour(gen_event).

-export([init/1, handle_event/2]).

init(_Args) ->
    {ok, []}.

handle_event(Event, _State) ->
    {io:write(Event), _State}.
