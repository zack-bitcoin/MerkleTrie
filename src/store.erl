-module(store).
-export([store/3]).

store(P, Value, Root) -> %returns {RootHash, RootPointer, Proof}
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    Leaf = <<P/binary, Value/binary>>,
    LPointer = dump:write(Leaf, leaf),
    LH = hash:doit(Leaf),
    case find_branch(P, 0, Value, Root, []) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems to Branch to make NewBranch.
	    H = more_branch(Leaf, LP2, Leaf2,length(Branch)),
	    store_branch(H++Branch, P, 2, LPointer, LH);
	Branch -> %overwrite
	    store_branch(Branch, P, 2, LPointer, LH)
    end.
find_branch(Path, N, Value, Parent, Trail) ->
    %gather the branch as it currently looks.
    NN = 4*N,
    <<_:NN, A:4, _/bitstring>> = Path,
    M = N+1,
    R = dump:get(Parent, stem),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    case stem:type(A+1, R) of
	0 ->%empty
	    RP;
	1 ->%another stem
	    find_branch(Path, M, Value, Pointer, RP);
	2 ->%a leaf. 
	    %io:fwrite("a leaf\n"),
	    L = dump:put(Pointer, leaf),
	    case hash:doit(L) of
		Path -> %overwrite
		    RP;
		_ -> %split leaf, add stem(s)
		    {L, Pointer, RP}
	    end
    end.
store_branch([], Path, _Type, Pointer, _) ->
    {Hash, _, _, Proof} = get:get(Path, Pointer),
    {Hash, Pointer, Proof};
store_branch([B|Branch], Path, Type, Pointer, Hash) ->
    %S = length(Branch)+1,
    S = length(Branch),
    NN = 4*S,
    <<_:NN, A:4, _/bitstring>> = Path,
    S1 = stem:add(B, A, Type, Pointer, Hash),
    Loc = dump:put(S1, stem),
    SH = stem:hash(S1),
    store_branch(Branch, Path, 1, Loc, SH).
more_branch(Leaf, LP2, Leaf2, B) ->
    {A, N2} = more_branch3(Leaf, Leaf2, 0),
    [H|T] = more_branch2(A-B+1),
    LH2 = hash:doit(Leaf),
    H2 = stem:add(H, N2, 2, LP2, LH2),
    [H2|T].
more_branch3(Leaf, Leaf2, N) ->
    NN = N*4,
    <<_:NN, A:4, _/bitstring>> = Leaf,
    <<_:NN, B:4, _/bitstring>> = Leaf2,
    if
	A == B -> more_branch3(Leaf, Leaf2, N+1);
	true -> {N, B}
    end.
more_branch2(0) -> [];
more_branch2(N) -> [stem:new_empty()|more_branch2(N-1)].
