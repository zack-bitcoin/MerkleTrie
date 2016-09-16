-module(verify).
-export([proof/4]).
proof(RootHash, L, Proof, CFG) ->
    [H|F] = flip(Proof),
    SH = stem:hash(H, CFG),
    if
	SH == RootHash ->
	    verify_proof2(leaf:path(L, CFG), L, [H|F], CFG);
	true -> 
	    io:fwrite("false 1\n"),
	    false
    end.
verify_proof2(<<N:4, _/bitstring>>, Leaf, P, CFG) when length(P) == 1->
    P1 = hd(P),
    Hash = element(N+1, P1),
    Hash == leaf:hash(Leaf, CFG);
verify_proof2(<<N:4, Path/bitstring>>, Leaf, [P1|[P2|Proof]], CFG) ->
    Hash = element(N+1, P1),
    case stem:hash(P2, CFG) of
	Hash -> verify_proof2(Path, Leaf, [P2|Proof], CFG);
	X ->
	    io:fwrite(X),
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

