-module(ram_store).
-export([store/3]).

store(Leaf, Trie, CFG) ->
    P = leaf:path(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Weight = leaf:weight(Leaf),
    B = case get_branch(P, 0, leaf:value(Leaf), Trie, [], CFG) of
	{Leaf2, LP2, Branch} -> %split leaf, add stem(s)
	    {A, N2} = path_match(P, leaf:path(Leaf2, CFG), 0),
	    [H|T] = empty_stems(A-length(Branch)+1),
	    LH2 = leaf:hash(Leaf2, CFG),
	    W2 = leaf:weight(Leaf2),
	    H2 = stem:add(H, N2, 2, LP2, W2, LH2),
	    [H2|T]++Branch;
	Branch -> %overwrite
	    Branch
    end,
    store_branch(B, P, 2, Leaf, CFG).

get_branch(_,_,_,_,_,_) ->
    ok.
store_branch(_,_,_,_,_) ->
    ok.

add(L) -> add(L, 0).
add([], X) -> X;
add([H|T], X) -> add(T, H+X).
path_match(LP, LP2, N) -> %returns {convergense_length, next nibble}
    NN = N*4,
    <<_:NN, A:4, _/bitstring>> = LP,
    <<_:NN, B:4, _/bitstring>> = LP2,
    if
	A == B -> path_match(LP, LP2, N+1);
	true -> {N, B}%+leaf:weight(Leaf)}
    end.
empty_stems(0) -> [];
empty_stems(N) -> [stem:new_empty()|empty_stems(N-1)].
