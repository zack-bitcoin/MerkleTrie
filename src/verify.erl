-module(verify).
-export([proof/4]).

-spec proof(stem:hash(), leaf:leaf(), get:proof(), cfg:cfg()) -> boolean().
proof(RootHash, L, Proof, CFG) ->
    [H|F] = lists:reverse(Proof),
    SH = stem:hash(H, CFG),
    if
	SH == RootHash ->
	    proof_internal(leaf:path(L, CFG), L, [H|F], CFG);
	true -> 
	    io:fwrite("false 1\n"),
	    false
    end.

-spec proof_internal(leaf:path(), leaf:leaf(), get:proof(), cfg:cfg()) -> boolean().
proof_internal([<<N:4>> | _], Leaf, P, CFG) when length(P) == 1->
    P1 = hd(P),
    Hash = element(N+1, P1),
    Hash == leaf:hash(Leaf, CFG);
proof_internal([<<N:4>>| Path ], Leaf, [P1, P2 | Proof], CFG) ->
    Hash = element(N+1, P1),
    case stem:hash(P2, CFG) of
	Hash -> proof_internal(Path, Leaf, [P2 | Proof], CFG);
	X ->
	    io:fwrite(X),
	    io:fwrite("false 3\n"),
	    false
    end;
proof_internal(_, _, _, _) ->
    io:fwrite("false 2\n"),
    false.

