-module(store).
-export([store/3]).

store(Leaf, Root, CFG) -> %returns {RootHash, RootPointer, Proof}
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Weight = leaf:weight(Leaf),
    P = leaf:path(Leaf, CFG),
    case find_branch(P, 0, leaf:value(Leaf), Root, [], CFG) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems to Branch to make NewBranch.
	    H = more_branch(Leaf, LP2, Leaf2,length(Branch),CFG),
	    store_branch(H++Branch, P, 2, LPointer, LH, Weight, CFG);
	Branch -> %overwrite
	    store_branch(Branch, P, 2, LPointer, LH, Weight, CFG)
    end.
find_branch(Path, N, Value, Parent, Trail, CFG) ->
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
	    find_branch(Path, M, Value, Pointer, RP, CFG);
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
store_branch([], Path, _Type, Pointer, _, _, CFG) ->
    {Hash, _, Proof} = get:get(Path, Pointer, CFG),
    {Hash, Pointer, Proof};
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
more_branch(Leaf, LP2, Leaf2, B, CFG) ->
    {A, N2, Weight2} = more_branch3(Leaf, Leaf2, 0, CFG),
    [H|T] = more_branch2(A-B+1),
    LH2 = leaf:hash(Leaf2, CFG),
    H2 = stem:add(H, N2, 2, LP2, Weight2, LH2),
    [H2|T].
more_branch3(Leaf, Leaf2, N, CFG) -> %returns {convergense_length, next nibblei, Weight}
    NN = N*4,
    <<_:NN, A:4, _/bitstring>> = leaf:path(Leaf, CFG),
    <<_:NN, B:4, _/bitstring>> = leaf:path(Leaf2, CFG),
    if
	A == B -> more_branch3(Leaf, Leaf2, N+1, CFG);
	true -> {N, B, leaf:weight(Leaf2)}%+leaf:weight(Leaf)}
    end.
more_branch2(0) -> [];
more_branch2(N) -> [stem:new_empty()|more_branch2(N-1)].
