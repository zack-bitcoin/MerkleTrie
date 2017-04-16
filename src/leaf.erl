-module(leaf).
-compile(export_all).
-record(leaf, {key = 0, value = 0, 
	       meta = 0}). %meta is data we want to remember that doesn't get hashed into the merkle tree.
serialize(X, CFG) ->
    P = cfg:path(CFG) * 8,
    M = cfg:meta(CFG) * 8,
    S = cfg:value(CFG) * 8,
    S = size(X#leaf.value) * 8,
    %io:fwrite({CFG, X}),
    <<(X#leaf.key):P, (X#leaf.value)/binary, (X#leaf.meta):M>>.
deserialize(A, CFG) ->
    L = cfg:value(CFG) * 8,
    P = cfg:path(CFG) * 8,
    MS = cfg:meta(CFG) * 8,
    <<Key:P, Value:L, Meta:MS>> = A,
    #leaf{key = Key, value = <<Value:L>>, meta = Meta}. 
new(Key, Value, Meta, CFG) ->
    true = Key > 0,
    L = cfg:value(CFG) * 8,
    <<_:L>> = Value,
    #leaf{key = Key, value = Value, meta = Meta}. 
key(L) -> L#leaf.key.
path(L, CFG) ->
    K = key(L),
    path_maker(K, CFG).
path_maker(K, CFG) ->
    T = cfg:path(CFG)*8,
    flip_bytes(<<K:T>>).
value(L) -> L#leaf.value.
meta(X) -> X#leaf.meta.
put(Leaf, CFG) ->
    dump:put(serialize(Leaf, CFG), 
	     ids:leaf(CFG)).
get(Pointer, CFG) ->
    L = dump:get(Pointer, ids:leaf(CFG)),
    deserialize(L, CFG).
hash(L, CFG) ->   
    P = cfg:path(CFG) * 8,
    HS = cfg:hash_size(CFG),
    hash:doit(<<(L#leaf.key):P, (L#leaf.value)/binary>>, HS).
flip_bytes(X) -> flip_bytes(X, <<>>).
flip_bytes(<<>>, X) -> X;
flip_bytes(<<N:4, T/bitstring>>, X) -> 
    flip_bytes(T, <<N:4, X/bitstring>>).
