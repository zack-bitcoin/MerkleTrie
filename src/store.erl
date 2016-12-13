-module(store).
-export([store/3, store/5]).

store(Leaf, Hash, Proof, Root, CFG) -> %this restores information to the merkle trie that had been garbage collected.

    %This function has a problem. It creates a new small trie containing JUST the proof. We want to integrate the proof with our existing trie.
    %We should probably calculate the existing branch, and the proof2branch, and mix them together. We want the new branch to contain pointers to the existing data.

    true = verify:proof(Hash, Leaf, Proof, CFG),
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Weight = leaf:weight(Leaf),
    Path = leaf:path(Leaf, CFG),
    Branch = proof2branch(Proof, 2, LPointer, LH, Path, CFG),
    V = leaf:value(Leaf),
    Branch2 = get_branch(Path, 0, V, Root, [], CFG),
    Branch3 = combine_branches(Path, Branch, Branch2),
    store_branch(Branch3, Path, 2, LPointer, LH, Weight, CFG).
combine_branches(_, X, []) -> X;
combine_branches(<<N:4, Path/bitstring>>, [Sa|A], [Sb|B]) ->%The second one has many pointers we care about. The first one has 1 leaf-pointer we care about.
    %io:fwrite([Sa, Sb]),
    [combine_stems(N+1, Sa, Sb)|combine_branches(Path, A, B)].
combine_stems(N, A, B) ->
    %io:fwrite([A, B]),
    T = stem:type(N, A),
    P = stem:pointer(N, A),
    H = element(N, stem:hashes(A)),
    stem:add(B, N-1, T, P, 0, H).
    
proof2branch([],_,_,_, _, _) -> [];
proof2branch([H|T], Type, Pointer, Hash, Path, CFG) -> 
    <<Nibble:4, NewPath/bitstring>> = Path,
    S = stem:recover(Nibble, Type, Pointer, 0, Hash, H),
    NewPointer = stem:put(S, CFG),
    NewHash = stem:hash(S, CFG),
    [S|proof2branch(T, 1, NewPointer, NewHash, NewPath, CFG)].
    
    
store(Leaf, Root, CFG) -> %returns {RootHash, RootPointer, Proof}
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Weight = leaf:weight(Leaf),
    P = leaf:path(Leaf, CFG),
    B = case get_branch(P, 0, leaf:value(Leaf), Root, [], CFG) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems.
		{A, N2} = path_match(P, leaf:path(Leaf2, CFG), 0),
		[H|T] = empty_stems(A-length(Branch)+1),
		LH2 = leaf:hash(Leaf2, CFG),
		W2 = leaf:weight(Leaf2),
		H2 = stem:add(H, N2, 2, LP2, W2, LH2),
		[H2|T]++Branch;
	    Branch -> %overwrite
		Branch
    end,
    store_branch(B, P, 2, LPointer, LH, Weight, CFG).
get_branch(Path, N, Value, Parent, Trail, CFG) ->
    %gather the branch as it currently looks.
    NN = 4*N,
    <<_:NN, A:4, _/bitstring>> = Path,
    M = N+1,
    R = stem:get(Parent, CFG),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    case stem:type(A+1, R) of
	0 ->%empty
	    RP;
	1 ->%another stem
	    get_branch(Path, M, Value, Pointer, RP, CFG);
	2 ->%a leaf. 
	    Leaf = leaf:get(Pointer, CFG),
	    case leaf:path(Leaf, CFG) of
		Path -> %overwrite
		    io:fwrite("overwrite\n"),
		    RP;
		_ -> %split leaf, add stem(s)
		    {Leaf, Pointer, RP}
	    end
    end.
store_branch([], Path, _, Pointer, _, _, CFG) ->
    %Instead of getting the thing, we can build it up while doing store.
    case get:get(Path, Pointer, CFG) of
	{Hash, _, Proof} -> {Hash, Pointer, Proof};
	empty -> store_branch([], Path, 0, Pointer, 0, 0, CFG)
    end;
store_branch([B|Branch], Path, Type, Pointer, Hash, Weight, CFG) ->
    S = length(Branch),
    NN = 4*S,
    <<_:NN, A:4, _/bitstring>> = Path,
    S1 = stem:add(B, A, Type, Pointer, Weight, Hash),
    Loc = stem:put(S1, CFG),
    SH = stem:hash(S1, CFG),
    NewWeight = add(tuple_to_list(stem:weights(S1))),
    store_branch(Branch, Path, 1, Loc, SH, NewWeight, CFG).
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
