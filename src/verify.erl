-module(verify).
-export([proof/6,proof/4]).

proof(RootHash, Key, Value, Weight, Proof, WS) ->
    %Pa = hash:doit(Key),
    proof(RootHash, <<Weight:WS, Key/binary, Value/binary>>, Proof, WS).
proof(RootHash, L, Proof, WS) ->
    [H|F] = flip(Proof),
    SH = stem:hash(H, WS),
    if
	SH == RootHash ->
	    verify_proof2(L, L, [H|F], WS);
	true -> 
	    io:fwrite("false 1\n"),
	    false
    end.
verify_proof2(<<N:4, _/bitstring>>, Leaf, P, _WS) when length(P) == 1->
    P1 = hd(P),
    Hash = element(N+1, P1),
    Hash == hash:doit(Leaf);
verify_proof2(<<N:4, Path/bitstring>>, Leaf, [P1|[P2|Proof]], WS) ->
    Hash = element(N+1, P1),
    %<<H:16, _/bitstring>> = Hash,
    case stem:hash(P2, WS) of
	Hash -> verify_proof2(Path, Leaf, [P2|Proof], WS);
	_ -> 
	    io:fwrite("false 3\n"),
	    false
    end;
verify_proof2(_, _, _, _) -> 
    io:fwrite("false 2\n"),
    false.
flip(X) -> flip(X, []).
flip([], X) -> X;
flip([A|B], C) -> 
    flip(B, [A|C]).

