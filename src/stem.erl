%The purpose of this file is to define stems as a data structure in ram, and give some simple functions to operate on them.

-module(stem).
-export([test/0,empty_root/1,serialize/2,deserialize/2,type/2,hash/2,pointers/1,types/1,hashes/1,pointer/2,new/5,add/6,new_empty/0,weight/2,weights/1]).
-record(stem, {types = empty_tuple(), pointers = empty_tuple(), weights = empty_tuple(), hashes = empty_hashes()}).
empty_tuple() -> {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}.
add(S, N, T, P, W, H) ->
    M = N+1,
    Ty = S#stem.types,
    Po = S#stem.pointers,
    Ha = S#stem.hashes,
    We = S#stem.weights,
    T2 = setelement(M, Ty, T),
    P2 = setelement(M, Po, P),
    H2 = setelement(M, Ha, H),
    W2 = setelement(M, We, W),
    #stem{types = T2, pointers = P2, hashes = H2, weights = W2}.
new_empty() -> #stem{}.
new(M, T, P, W, H) ->
    %N is the nibble being pointed to.
    %T is the type, P is the pointer, W is the amount of money on that branch, H is the Hash
    S = new_empty(),
    add(S, M, T, P, W, H).
pointers(R) -> R#stem.pointers.
types(R) -> R#stem.types.
hashes(R) -> R#stem.hashes.
weights(R) -> R#stem.weights.
pointer(N, R) ->
    T = pointers(R),
    element(N, T).
type(N, R) ->
    T = types(R),
    element(N, T).
weight(N, R) ->
    T = weights(R),
    element(N, T).
serialize(S, WS) ->
    P = S#stem.pointers,
    H = S#stem.hashes,
    T = S#stem.types,
    W = S#stem.weights,
    serialize(P, H, T, W, WS, 1).
serialize(_, _, _, _, _, N) when N>16 -> <<>>;
serialize(P, H, T, W, WS, N) -> %WS is the size of the weight element in bits.
    P1 = element(N, P),
    H1 = element(N, H),
    T1 = element(N, T),
    W1 = element(N, W),
    D = serialize(P, H, T, W, WS, N+1),
    << T1:2, P1:40, W1:WS, H1/binary, D/bitstring >>.
deserialize(B, WS) -> 
    X = empty_tuple(),
    deserialize(1,X,X,X,WS,X, B).
deserialize(17, T,P,W,_WS,H, <<>>) -> 
    #stem{types = T, pointers = P, hashes = H, weights = W};
deserialize(N, T0,P0,W0,WS,H0,X) ->
    <<T:2, P:40, W:WS, H:96, D/bitstring>> = X,
    T1 = setelement(N, T0, T),
    P1 = setelement(N, P0, P),
    W1 = setelement(N, W0, W),
    H1 = setelement(N, H0, <<H:96>>),
    deserialize(N+1, T1, P1, W1, WS, H1, D).
empty_hashes() ->
    {<<0:96>>,<<0:96>>,<<0:96>>,<<0:96>>,
     <<0:96>>,<<0:96>>,<<0:96>>,<<0:96>>,
     <<0:96>>,<<0:96>>,<<0:96>>,<<0:96>>,
     <<0:96>>,<<0:96>>,<<0:96>>,<<0:96>>}.
empty_root(WS) ->
    P = empty_tuple(),
    T = empty_tuple(),
    H = empty_hashes(),
    W = empty_tuple(),
    S = #stem{types = T, pointers = P, hashes = H, weights = W},
    serialize(S, WS).

hash(S, WS) when is_binary(S) ->
    hash(deserialize(S, WS), WS);
hash(S, _WS) when is_tuple(S) and (size(S) == 16)->    
    hash2(1, S, <<>>);
hash(S, _WS) ->    
    H = S#stem.hashes,
    hash2(1, H, <<>>).
hash2(17, _, X) -> hash:doit(X);
hash2(N, H, X) ->
    A = element(N, H),
    12 = size(A),
    hash2(N+1, H, <<A/binary, X/binary>>).

test() ->
    P = {6,5,4,3,7,8,9,4,5,3,2,6,7,8,3,4},
    T = {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    W = {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    H = empty_hashes(),
    WS = 1,
    S = #stem{types = T, pointers = P, hashes = H, weights = W},
    S2 = serialize(S, WS),
    S = deserialize(S2, WS),
    hash(S, WS),
    success.
    
