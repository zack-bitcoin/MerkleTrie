-module(verify).
-export([proof/4]).

-spec proof(stem:hash(), leaf:leaf(), get:proof(), cfg:cfg()) -> boolean().
proof(RootHash, L, Proof, CFG) ->
    [H|F] = lists:reverse(Proof),
    %[H|F] = Proof,
    SH = stem:hash(H, CFG),
    if
	SH == RootHash ->
	    proof_internal(leaf:path(L, CFG), L, [H|F], CFG);
	true -> 
	    io:fwrite("false 1\n"),
	    false
    end.

-spec proof_internal(leaf:path(), leaf:leaf(), get:proof(), cfg:cfg()) -> boolean().
proof_internal([<<N:4>> | M], Leaf, P, CFG) when length(P) == 1->
    P1 = hd(P),
    Hash = element(N+1, P1),
    V = leaf:value(Leaf),
    LH = leaf:hash(Leaf, CFG),
    Hash == LH;
proof_internal([<<N:4>>| Path ], Leaf, [P1, P2 | Proof], CFG) ->
    %if leaf is empty, and P2 is a leaf, then we do a different test.
    %pass if hash(leaf) is in P1, and N does _not_ point to leaf P2.
    LB = leaf:is_serialized_leaf(P2, CFG),
    LV = leaf:value(Leaf),
    if
	(LV == empty) and LB ->
	    Leaf2 = leaf:deserialize(P2, CFG),
	    LH = leaf:hash(Leaf2, CFG),
	    is_in(LH, tuple_to_list(P1)) 
		and not(get:same_end(leaf:path(Leaf2, CFG), 
				     [<<N:4>>|Path], 
				     CFG));
	true ->
	    Hash = element(N+1, P1),
	    case stem:hash(P2, CFG) of
		Hash -> proof_internal(Path, Leaf, [P2 | Proof], CFG);
		X ->
		    io:fwrite("false 3\n"),
		    %io:fwrite({X, Hash, [P1, P2|Proof]}),
		    false
	    end
    end;
proof_internal(_, _, _, _) ->
    io:fwrite("false 2\n"),
    false.
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [A|T]) -> is_in(X, T).
