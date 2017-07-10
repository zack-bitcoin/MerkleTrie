-module(leaf).
-export([new/4, key/1, value/1, meta/1, path/2, path_maker/2, hash/2, put/2, get/2, serialize/2, deserialize/2]).
-record(leaf, {key = 0, value = 0, 
	       meta = 0}). %meta is data we want to remember that doesn't get hashed into the merkle tree.
serialize(X, CFG) ->
    P = cfg:path(CFG) * 8,
    M = cfg:meta(CFG) * 8,
    S = cfg:value(CFG),
    S = size(X#leaf.value),
    %io:fwrite({CFG, X}),
    <<(X#leaf.key):P, 
      (X#leaf.meta):M,
      (X#leaf.value)/binary>>.
deserialize(A, CFG) ->
    L = cfg:value(CFG) * 8,
    P = cfg:path(CFG) * 8,
    MS = cfg:meta(CFG) * 8,
    <<Key:P, 
      Meta:MS,
      Value:L>> = A,
    #leaf{key = Key, value = <<Value:L>>, meta = Meta}. 
new(Key, Value, Meta, CFG) ->
    {ok, _} = {check_key(Key, cfg:path(CFG)), Key},
    L = cfg:value(CFG) * 8,
    <<_:L>> = Value,
    #leaf{key = Key, value = Value, meta = Meta}. 
check_key(Key, LBytes) when is_integer(Key),
			    Key >= 0,
			    Key < (1 bsl (LBytes * 8)) ->
    ok;
check_key(Key, _) when is_integer(Key) ->
    {error, key_out_of_range};
check_key(_, _) ->
    {error, key_not_integer}.
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
