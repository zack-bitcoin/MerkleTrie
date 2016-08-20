-module(stem).
-export([test/0,empty_root/0,serialize/1,deserialize/1,type/2,hash/1,pointers/1,types/1,hashes/1,pointer/2,new/4,add/5,new_empty/0]).
-record(stem, {types = empty_tuple(), pointers = empty_tuple(), hashes = empty_tuple_bytes()}).
empty_tuple() -> {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}.
empty_tuple_bytes() -> 
    {<<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>}.
add(S, N, T, P, H) ->
    M = N+1,
    Ty = S#stem.types,
    Po = S#stem.pointers,
    Ha = S#stem.hashes,
    T2 = setelement(M, Ty, T),
    P2 = setelement(M, Po, P),
    H2 = setelement(M, Ha, H),
    #stem{types = T2, pointers = P2, hashes = H2}.
new_empty() -> #stem{}.
new(M, T, P, H) ->
    %N is the nibble being pointed to.
    %T is the type, P is the pointer, H is the Hash
    S = new_empty(),
    add(S, M, T, P, H).
%N = M+1,
%T2 = setelement(N, empty_tuple(), T),
%P2 = setelement(N, empty_tuple(), P),
%H2 = setelement(N, empty_tuple_bytes(), H),
%#stem{types = T2, pointers = P2, hashes = H2}.
pointers(R) -> R#stem.pointers.
types(R) -> R#stem.types.
hashes(R) -> R#stem.hashes.
pointer(N, R) ->
    T = pointers(R),
    element(N, T).
type(N, R) ->
    T = types(R),
    element(N, T).
serialize(S) ->
    P = S#stem.pointers,
    H = S#stem.hashes,
    T = S#stem.types,
    serialize(P, H, T, 1).
serialize(_, _, _, N) when N>16 -> <<>>;
serialize(P, H, T, N) -> 
    P1 = element(N, P),
    H1 = element(N, H),
    T1 = element(N, T),
    D = serialize(P, H, T, N+1),
    S = size(H1),
    X = 32 - S,
    Y = <<0:(X*8)>>,
    << T1:2, P1:40, H1/binary, Y/binary, D/bitstring >>.
deserialize(B) -> 
    X = empty_tuple(),
    deserialize(1,X,X,X,B).
deserialize(17, T,P,H, <<>>) -> 
    #stem{types = T, pointers = P, hashes = H};
deserialize(N, T0,P0,H0, <<T:2, P:40, H:256, D/bitstring>>) ->    
    T1 = setelement(N, T0, T),
    P1 = setelement(N, P0, P),
    H1 = setelement(N, H0, <<H:256>>),
    deserialize(N+1, T1, P1, H1, D).
empty_hashes() ->
    {<<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>,
     <<0:256>>,<<0:256>>,<<0:256>>,<<0:256>>}.
    
empty_root() ->
    P = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    T = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    H = empty_hashes(),
    S = #stem{types = T, pointers = P, hashes = H},
    serialize(S).

hash(S) when is_binary(S) ->
    hash(deserialize(S));
hash(S) when is_tuple(S) and (size(S) == 16)->    
    hash2(1, S, <<>>);
hash(S) ->    
    H = S#stem.hashes,
    hash(H).
hash2(17, _, X) -> hash:doit(X);
hash2(N, H, X) ->
    A = element(N, H),
    32 = size(A),
    hash2(N+1, H, <<A/binary, X/binary>>).

test() ->
    P = {6,5,4,3,7,8,9,4,5,3,2,6,7,8,3,4},
    T = {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    H = empty_hashes(),
    S = #stem{types = T, pointers = P, hashes = H},
    S2 = serialize(S),
    S = deserialize(S2),
    hash(S),
    success.
    
