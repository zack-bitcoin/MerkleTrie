%The purpose of this file is to define stems as a data structure in ram, and give some simple functions to operate on them.

-module(stem).
-export([test/0,get/2,put/2,type/2,hash/2,pointers/1,types/1,hashes/1,pointer/2,new/5,add/5,new_empty/1,recover/5]).
-export_type([stem/0,types/0,empty_t/0,stem_t/0,leaf_t/0,pointers/0,empty_p/0,hashes/0,hash/0,empty_hash/0,stem_p/0]).
-record(stem, { types = empty_tuple() :: types()
	      , pointers = empty_tuple() :: pointers()
	      , hashes :: hashes()
	      }).
-opaque stem() :: #stem{}.
-type types() :: {t(),t(),t(),t(),
		  t(),t(),t(),t(),
		  t(),t(),t(),t(),
		  t(),t(),t(),t()}.
-type t() :: type().
-type type() :: empty_t() | stem_t() | leaf_t().
-type empty_t() :: 0.
-type stem_t() :: 1.
-type leaf_t() :: 2.
-type pointers() :: {p(),p(),p(),p(),
		     p(),p(),p(),p(),
		     p(),p(),p(),p(),
		     p(),p(),p(),p()}.
-type p() :: pointer().
-type pointer() :: empty_p() | stem_p() | leaf:leaf_p().
-type empty_p() :: 0.
-type hashes() :: {h(),h(),h(),h(),
		   h(),h(),h(),h(),
		   h(),h(),h(),h(),
		   h(),h(),h(),h()}.
-type h() :: hash().
-type hash() :: hash(cfg:hash_size()).
-type hash(_CfgHashSizeBytes) :: non_empty_binary(). % non-empty because configured hash size positive
-type empty_hash() :: hash().
-opaque stem_p() :: non_neg_integer().
-type nibble() :: 0..15.
-type non_empty_binary() :: <<_:8, _:_*8>>.
empty_tuple() -> {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}.
-spec add(stem(), nibble(), leaf_t(), leaf:leaf_p(), hash()) -> stem();
	 (stem(), nibble(), stem_t(), stem_p(), hash()) -> stem();
	 (stem(), nibble(), empty_t(), empty_p(), empty_hash()) -> stem().
add(S, N, T, P, H) ->
    M = N+1,
    Ty = S#stem.types,
    Po = S#stem.pointers,
    Ha = S#stem.hashes,
    T2 = setelement(M, Ty, T),
    P2 = setelement(M, Po, P),
    H2 = setelement(M, Ha, H),
    #stem{types = T2, pointers = P2, hashes = H2}.
-spec new_empty(cfg:cfg()) -> stem().
new_empty(CFG) -> #stem{hashes = empty_hashes(CFG)}.
recover(M, T, P, H, Hashes) ->
    S = #stem{hashes = Hashes},
    add(S, M, T, P, H).
new(M, T, P, H, CFG) ->
    %N is the nibble being pointed to.
    %T is the type, P is the pointer, H is the Hash
    S = new_empty(CFG),
    add(S, M, T, P, H).
-spec pointers(stem()) -> pointers().
pointers(R) -> R#stem.pointers.
-spec types(stem()) -> types().
types(R) -> R#stem.types.
-spec hashes(stem()) -> hashes().
hashes(R) -> R#stem.hashes.
-spec pointer(1..16, stem()) -> pointer().
pointer(N, R) ->
    T = pointers(R),
    element(N, T).
-spec type(1..16, stem()) -> type().
type(N, R) ->
    T = types(R),
    element(N, T).
serialize(S, CFG) ->
    Path = cfg:path(CFG)*8,
    P = S#stem.pointers,
    H = S#stem.hashes,
    T = S#stem.types,
    X = serialize(P, H, T, Path, 1),
    X.
serialize(_, _, _, _, N) when N>16 -> <<>>;
serialize(P, H, T, Path, N) -> 
    P1 = element(N, P),
    H1 = element(N, H),
    T1 = element(N, T),
    D = serialize(P, H, T, Path, N+1),
    << T1:2, P1:Path, H1/binary, D/bitstring >>.
deserialize(B, CFG) -> 
    X = empty_tuple(),
    %deserialize(1,X,X,cfg:path(CFG)*8,hash:hash_depth()*8,X, B).
    HS = cfg:hash_size(CFG),
    deserialize(1,X,X,cfg:path(CFG)*8,HS*8,X, B).
deserialize(17, T,P,_,_,H, <<>>) -> 
    #stem{types = T, pointers = P, hashes = H};
deserialize(N, T0,P0,Path,HashDepth,H0,X) when N < 17 ->
    <<T:2, P:Path, H:HashDepth, D/bitstring>> = X,
    T1 = setelement(N, T0, T),
    P1 = setelement(N, P0, P),
    H1 = setelement(N, H0, <<H:HashDepth>>),
    deserialize(N+1, T1, P1, Path, HashDepth,H1, D).
empty_hashes(CFG) ->
    HS = cfg:hash_size(CFG),
    %X = hash:hash_depth()*8,
    X = HS * 8,
    {<<0:X>>,<<0:X>>,<<0:X>>,<<0:X>>,
     <<0:X>>,<<0:X>>,<<0:X>>,<<0:X>>,
     <<0:X>>,<<0:X>>,<<0:X>>,<<0:X>>,
     <<0:X>>,<<0:X>>,<<0:X>>,<<0:X>>}.

-spec hash(Hashes, cfg:cfg()) -> hash() when
      Hashes :: SerializedStem | hashes() | stem(),
      SerializedStem :: binary().
hash(S, CFG) when is_binary(S) ->
    hash(deserialize(S, CFG), CFG);
hash(S, CFG) when is_tuple(S) and (size(S) == 16)->    
    hash2(1, S, <<>>, CFG);
hash(S, CFG) ->    
    H = S#stem.hashes,
    hash2(1, H, <<>>, CFG).
hash2(17, _, X, CFG) -> 
    HS = cfg:hash_size(CFG),
    hash:doit(X, HS);
hash2(N, H, X, CFG) ->
    A = element(N, H),
    HS = cfg:hash_size(CFG),
    %12 = size(A),
    HS = size(A),
    hash2(N+1, H, <<A/binary, X/binary>>, CFG).
-spec put(stem(), cfg:cfg()) -> stem_p().
put(Stem, CFG) ->
    dump:put(serialize(Stem, CFG), ids:stem(CFG)).
-spec get(stem_p(), cfg:cfg()) -> stem().
get(Pointer, CFG) -> 
    S = dump:get(Pointer, ids:stem(CFG)),
    deserialize(S, CFG).
test() ->
    P = {6,5,4,3,7,8,9,4,5,3,2,6,7,8,3,4},
    T = {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    CFG = cfg:new(1, 9, 2, trie),
    H = empty_hashes(CFG),
    S = #stem{types = T, pointers = P, hashes = H},
    S2 = serialize(S, CFG),
    S = deserialize(S2, CFG),
    hash(S, CFG),
    success.
    
