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
    %Max = 20000000000,
    ID = trie01,
    WS = 0,
    KeyLength = 5,
    trie_sup:start_link(Size, ID, WS, KeyLength).

stop(_State) ->
    ok.
