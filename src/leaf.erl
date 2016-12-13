-module(leaf).
-compile(export_all).
-record(leaf, {key = 0, value = 0}).
serialize(X, CFG) ->
    S = cfg:value(CFG),
    P = cfg:path(CFG) * 8,
    S = size(X#leaf.value),
    
    <<(X#leaf.key):P, (X#leaf.value)/binary>>.
deserialize(A, CFG) ->
    L = cfg:value(CFG) * 8,
    P = cfg:path(CFG) * 8,
    <<Key:P, Value:L>> = A,
    #leaf{key = Key, value = <<Value:L>>}. 
new(Key, Value, CFG) ->
    true = Key > 0,
    L = cfg:value(CFG) * 8,
    <<_:L>> = Value,
    #leaf{key = Key, value = Value}. 
key(L) -> L#leaf.key.
path(L, CFG) ->
    K = key(L),
    path_maker(K, CFG).
path_maker(K, CFG) ->
    T = cfg:path(CFG)*8,
    flip_bytes(<<K:T>>).
value(L) -> L#leaf.value.
put(Leaf, CFG) ->
    dump:put(serialize(Leaf, CFG), 
	     ids:leaf(CFG)).
get(Pointer, CFG) ->
    L = dump:get(Pointer, ids:leaf(CFG)),
    deserialize(L, CFG).
hash(L, CFG) ->   
    hash:doit(serialize(L, CFG)).
flip_bytes(X) -> flip_bytes(X, <<>>).
flip_bytes(<<>>, X) -> X;
flip_bytes(<<N:4, T/bitstring>>, X) -> 
    flip_bytes(T, <<N:4, X/bitstring>>).
