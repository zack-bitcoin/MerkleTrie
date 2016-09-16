-module(trie_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%start(_StartType, _StartArgs) ->
start(normal, []) ->
    Size = 2,
    %Max = 20000000000,
    ID = trie01,
    WS = 1,
    KeyLength = 5,
    trie_sup:start_link(WS, KeyLength, Size, ID).

stop(_State) ->
    ok.
