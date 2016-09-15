-module(leaf).
-compile(export_all).
-export([path/2,new/3,key/1,value/1,weight/1,put/4,hash/3,path_maker/2,serialize/3,deserialize/3]).
-record(leaf, {key = 0, weight = 0, value = 0}).
serialize(X, WS, LS) ->
    %L = LS*8,
    W = WS*8,
    L = (LS * 8) - W - 40,
    <<(X#leaf.key):40, (X#leaf.weight):W, (X#leaf.value):L>>.
deserialize(A, WS, LS) ->
    W = WS * 8,
    L = (LS * 8) - W - 40,
    <<Key:40, Weight:W, Value:L>> = A,
    #leaf{key = Key, weight = Weight, value = Value}. 
    
new(Key, Weight, Value) ->
    #leaf{key = Key, weight = Weight, value = Value}. 
key(L) -> L#leaf.key.
path(L, S) ->%S is the size of hash:doit(1)'s output in bytes.
    K = key(L),
    path_maker(K, S).
path_maker(K, S) ->
    T = S*8,
    flip_bytes(<<K:T>>).
value(L) -> L#leaf.value.
weight(L) ->
    L#leaf.weight.
put(Leaf, WS, LS, ID) ->
    dump:put(serialize(Leaf, WS, LS), ids:leaf(ID)).
get(Pointer, WS, LS, ID) ->
    L = dump:get(Pointer, ids:leaf(ID)),
    deserialize(L, WS, LS).
hash(L, WS, LS) ->   
    hash:doit(serialize(L, WS, LS)).
flip_bytes(X) -> flip_bytes(X, <<>>).
flip_bytes(<<>>, X) -> X;
flip_bytes(<<N:8, T/bitstring>>, X) -> 
    flip_bytes(T, <<N:8, X/bitstring>>).
