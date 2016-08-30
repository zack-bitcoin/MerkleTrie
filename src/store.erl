-module(store).
-export([store/4]).

store(P, Value, Root, ID) when is_binary(P)-> %returns {RootHash, RootPointer, Proof}
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    Leaf = <<P/binary, Value/binary>>,
    LPointer = dump:put(Leaf, ids:leaf(ID)),
    LH = hash:doit(Leaf),
    case find_branch(P, 0, Value, Root, [], ID) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems to Branch to make NewBranch.
	    H = more_branch(Leaf, LP2, Leaf2,length(Branch)),
	    store_branch(H++Branch, P, 2, LPointer, LH, ID);
	Branch -> %overwrite
	    store_branch(Branch, P, 2, LPointer, LH, ID)
    end;
store(P, Value, Root, ID) -> 
    P2 = stem:serialize(P),
    store(P2, Value, Root, ID).
find_branch(Path, N, Value, Parent, Trail, ID) ->
    %gather the branch as it currently looks.
    NN = 4*N,
    <<_:NN, A:4, _/bitstring>> = Path,
    M = N+1,
    R = stem:deserialize(dump:get(Parent, ids:stem(ID))),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    case stem:type(A+1, R) of
	0 ->%empty
	    RP;
	1 ->%another stem
	    find_branch(Path, M, Value, Pointer, RP, ID);
	2 ->%a leaf. 
	    %io:fwrite("a leaf\n"),
	    Leaf = dump:get(Pointer, ids:leaf(ID)),
	    <<L:40, _/bitstring>> = Leaf,
	    La = <<L:40>>,
	    case La of
		Path -> %overwrite
		    io:fwrite("overwrite\n"),
		    RP;
		_ -> %split leaf, add stem(s)
		    {Leaf, Pointer, RP}
	    end
    end.
store_branch([], Path, _Type, Pointer, _, ID) ->
    {Hash, _, _, Proof} = get:get(Path, Pointer, ID),
    {Hash, Pointer, Proof};
store_branch([B|Branch], Path, Type, Pointer, Hash, ID) ->
    %S = length(Branch)+1,
    S = length(Branch),
    NN = 4*S,
    <<_:NN, A:4, _/bitstring>> = Path,
    S1 = stem:add(B, A, Type, Pointer, Hash),
    Loc = dump:put(stem:serialize(S1), ids:stem(ID)),
    SH = stem:hash(S1),
    store_branch(Branch, Path, 1, Loc, SH, ID).
more_branch(Leaf, LP2, Leaf2, B) ->
    {A, N2} = more_branch3(Leaf, Leaf2, 0),
    [H|T] = more_branch2(A-B+1),
    LH2 = hash:doit(Leaf2),
    H2 = stem:add(H, N2, 2, LP2, LH2),
    [H2|T].
more_branch3(Leaf, Leaf2, N) -> %returns {convergense_length, next nibble}
    NN = N*4,
    <<_:NN, A:4, _/bitstring>> = Leaf,
    <<_:NN, B:4, _/bitstring>> = Leaf2,
    if
	A == B -> more_branch3(Leaf, Leaf2, N+1);
	true -> {N, B}
    end.
more_branch2(0) -> [];
more_branch2(N) -> [stem:new_empty()|more_branch2(N-1)].
