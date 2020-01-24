-module(trie_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%start(_StartType, _StartArgs) ->
start(normal, []) ->
    Size = 2,
    %Max = 20000000000,
    ID = trie01,
    KeyLength = 5,%in bytes
    Amount = 1000000,
    Mode = ram,
    Meta = 2,
    HashSize = 32,
    trie_sup:start_link(KeyLength, Size, ID, Amount, Meta, HashSize, Mode, "temp.db").

stop(_State) ->
    ok.
