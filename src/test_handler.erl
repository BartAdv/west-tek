-module(test_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2]).

init(_Args) ->
    {ok, []}.

handle_event(Event, Es) ->
    {ok, Es ++ [Event]}.

handle_call(dump, Es) ->
    {ok, Es, Es}.
