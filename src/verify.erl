-module(verify).
-export([proof/4, update_proof/3, update_proofs/2]).

-spec proof(stem:hash(), leaf:leaf(), get:proof(), cfg:cfg()) -> boolean().

update_proof(L, Proof, CFG) ->
    LP = leaf:path(L, CFG),
    %take the slice of the path we will use, and reverse it.
    N = length(Proof),
    {LP2, _} = lists:split(N, LP),
    LP3 = lists:reverse(LP2),
    LH = leaf:hash(L, CFG),
    Proof2 = update_internal(LP3, LH, Proof, CFG),
    Proof2.

update_internal(_, _, [], _) -> [];
update_internal([<<N:4>> | M], LH, Proof, CFG) ->
    P1 = hd(Proof),
    %Hash = element(N+1, P1),
    P2 = setelement(N+1, P1, LH),
    NH = stem:hash(P2, CFG),
    [P2|update_internal(M, NH, tl(Proof), CFG)].

update_proofs(X, CFG) ->
    update_proofs(X, CFG, dict:new(), []).
update_proofs([], _, D, L) ->
    L2 = lists:reverse(L),
    lists:map(fun(X) ->%do this to every list in the list of lists.
		      lists:map(fun(Y) ->%update every element of the list
					merge_find_helper(Y, D)

				end, X)
	      end, L2);
update_proofs([{Leaf, Proof}|T], CFG, D, L) ->
    %use D to remember which stems have been updated already.
    LP = leaf:path(Leaf, CFG),
    N = length(Proof),
    {LP2, _} = lists:split(N, LP),
    LP3 = lists:reverse(LP2),
    LH = leaf:hash(Leaf, CFG),
    {D2, NewProof} = update_proofs2(LP3, LH, Proof, D, CFG, []),
    update_proofs(T, CFG, D2, [NewProof|L]).
    
merge_find_helper(P, D) ->
    io:fwrite("merge find helper\n"),
    case dict:find(P, D) of
	error -> P;
	{ok, error} -> 1=2;
	{ok, P2} ->
	    merge_find_helper(P2, D)
    end.

update_proofs2(_, _, [], D, _, Proof) -> 
    {D, lists:reverse(Proof)};
update_proofs2([<<N:4>>|M], LH, Proof, D, CFG, Proof2) -> 
    P1 = hd(Proof),
    P = merge_find_helper(P1, D),
    P2 = setelement(N+1, P, LH),
    D2 = dict:store(P1, P2, D),
    D3 = dict:store(P, P2, D),
    NH = stem:hash(P2, CFG),
    update_proofs2(M, NH, tl(Proof), D3, CFG, [P2|Proof2]).

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
