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
    Amount = 1000000,
    Mode = hd,
    trie_sup:start_link(WS, KeyLength, Size, ID, Amount, Mode).

stop(_State) ->
    ok.
