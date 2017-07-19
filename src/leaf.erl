-module(leaf).
-export([new/4, key/1, value/1, meta/1, path/2, path_maker/2, hash/2, put/2, get/2, serialize/2, deserialize/2]).
-export_type([leaf/0,key/0,value/0,meta/0,leaf_p/0,path/0]).
-record(leaf, { key :: key()
	      , value :: value()
	      , meta :: meta() %meta is data we want to remember that doesn't get hashed into the merkle tree.
	      }).
-opaque leaf() :: #leaf{}.
-type key() :: non_neg_integer().
-type value() :: binary().
-type meta() :: non_neg_integer().
-opaque leaf_p() :: non_neg_integer().
-type path() :: path(cfg:path()).
-type path(_CfgPathSizeBytes) :: [nib(), ...]. % non-empty because configured path size positive
-type nib() :: <<_:4>>.

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
-spec new(key(), value(), meta(), cfg:cfg()) -> leaf().
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
-spec path(leaf(), cfg:cfg()) -> path().
path(L, CFG) ->
    K = key(L),
    path_maker(K, CFG).
-spec path_maker(key(), cfg:cfg()) -> path().
path_maker(K, CFG) ->
    T = cfg:path(CFG)*8,
    lists:reverse([<<N:4>>||<<N:4>> <= <<K:T>>]).

value(L) -> L#leaf.value.
meta(X) -> X#leaf.meta.
-spec put(leaf(), cfg:cfg()) -> leaf_p().
put(Leaf, CFG) ->
    dump:put(serialize(Leaf, CFG), 
	     ids:leaf(CFG)).
-spec get(leaf_p(), cfg:cfg()) -> leaf().
get(Pointer, CFG) ->
    L = dump:get(Pointer, ids:leaf(CFG)),
    deserialize(L, CFG).
-spec hash(leaf(), cfg:cfg()) -> stem:hash().
hash(L, CFG) ->   
    P = cfg:path(CFG) * 8,
    HS = cfg:hash_size(CFG),
    hash:doit(<<(L#leaf.key):P, (L#leaf.value)/binary>>, HS).
