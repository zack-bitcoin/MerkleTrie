
-module(verify).
-export([proof/6]).

%proof(RootHash, Key, Value, Weight, Proof, WS) ->
    %Pa = hash:doit(Key),
    %proof(RootHash, <<Weight:(8*WS), Key/binary, Value/binary>>, Proof, WS).
proof(RootHash, L, Proof, WS, LS, KeyLength) ->
    [H|F] = flip(Proof),
    SH = stem:hash(H, WS),
    if
	SH == RootHash ->
	    verify_proof2(leaf:path(L, KeyLength), L, [H|F], WS, LS);
	true -> 
	    io:fwrite("false 1\n"),
	    false
    end.
verify_proof2(<<N:4, _/bitstring>>, Leaf, P, WS, LS) when length(P) == 1->
    P1 = hd(P),
    Hash = element(N+1, P1),
    Hash == leaf:hash(Leaf, WS, LS);
verify_proof2(<<N:4, Path/bitstring>>, Leaf, [P1|[P2|Proof]], WS, LS) ->
    Hash = element(N+1, P1),
    case stem:hash(P2, WS) of
	Hash -> verify_proof2(Path, Leaf, [P2|Proof], WS, LS);
	X ->
	    io:fwrite(X),
	    io:fwrite("false 3\n"),
	    false
    end;
verify_proof2(_, _, _, _, _) -> 
    io:fwrite("false 2\n"),
    false.
flip(X) -> flip(X, []).
flip([], X) -> X;
flip([A|B], C) -> 
    flip(B, [A|C]).

