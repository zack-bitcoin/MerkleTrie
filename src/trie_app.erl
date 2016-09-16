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
    CFG = cfg:new(WS, KeyLength, Size, ID),
    trie_sup:start_link(CFG).

stop(_State) ->
    ok.
