-module(dump_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dump_sup:start_link(dump, 32).

stop(_State) ->
    ok.
