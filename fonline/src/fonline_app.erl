-module(fonline_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    locations:init("../fonline/Server"),
    ok.

stop(_State) ->
    ok.
