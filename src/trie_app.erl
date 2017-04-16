-module(trie_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%start(_StartType, _StartArgs) ->
start(normal, []) ->
    Size = 2,
    %Max = 20000000000,
    ID = trie01,
    KeyLength = 9,
    Amount = 1000000,
    Mode = hd,
    Meta = 2,
    HashSize = 12,
    trie_sup:start_link(KeyLength, Size, ID, Amount, Meta, HashSize, Mode).

stop(_State) ->
    ok.
