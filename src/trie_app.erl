-module(trie_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%start(_StartType, _StartArgs) ->
start(normal, []) ->
    Size = 2,
    Max = 20000000000,
    ID = trie01,
    trie_sup:start_link(Size, Max, ID).

stop(_State) ->
    ok.
