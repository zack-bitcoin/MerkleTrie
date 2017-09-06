-module(store).
-export([store/3, restore/5, get_branch/5, store_branch/6]).
-export_type([branch/0, nonempty_branch/0]).

-type branch() :: [stem:stem()]. % first element is most distant from root i.e. closest to leaf (if any)
-type nonempty_branch() :: [stem:stem(), ...].

restore(Leaf, Hash, Proof, Root, CFG) -> %this restores information to the merkle trie that had been garbage collected.

    %We should probably calculate the existing branch, and the proof2branch, and mix them together. We want the new branch to contain pointers to the existing data.

    true = verify:proof(Hash, Leaf, Proof, CFG),
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    Path = leaf:path(Leaf, CFG),
    Branch = proof2branch(Proof, 2, LPointer, LH, Path, CFG),
    Branch2 = get_branch(Path, 0, Root, [], CFG),
    Branch3 = combine_branches(Path, Branch, Branch2),
    store_branch(Branch3, Path, 2, LPointer, LH, CFG).
combine_branches(_, X, []) -> X;
combine_branches([<<N:4>> | Path], [Sa|A], [Sb|B]) ->%The second one has many pointers we care about. The first one has 1 leaf-pointer we care about.
    %[combine_stems(N+1, Sa, Sb) | combine_branches(Path, A, B)].
    [combine_stems(N+1, Sa, Sb) | combine_branches(Path, A, B)].
combine_stems(N, A, B) ->
    T = stem:type(N, A),
    P = stem:pointer(N, A),
    H = element(N, stem:hashes(A)),
    stem:add(B, N-1, T, P, H).
    
proof2branch([],_,_,_, _, _) -> [];
proof2branch([H|T], Type, Pointer, Hash, Path, CFG) -> 
    [<<Nibble:4>> | NewPath] = Path,
    S = stem:recover(Nibble+1, Type, Pointer, Hash, H),
    NewPointer = stem:put(S, CFG),
    NewHash = stem:hash(S, CFG),
    [S|proof2branch(T, 1, NewPointer, NewHash, NewPath, CFG)].
    
    
-spec store(leaf:leaf(), stem:stem_p(), cfg:cfg()) ->
		   {RootHash, RootPointer, get:proof()}
		       when RootHash :: stem:hash(),
			    RootPointer :: stem:stem_p().
store(Leaf, Root, CFG) ->
    %we could make it faster if the input was like [{Key1, Value1}, {Key2, Value2}...]
    LPointer = leaf:put(Leaf, CFG),
    LH = leaf:hash(Leaf, CFG),
    P = leaf:path(Leaf, CFG),
    B = case get_branch(P, 0, Root, [], CFG) of
	{Leaf2, LP2, Branch} ->%split leaf, add stem(s)
	    %need to add 1 or more stems.
		{A, N2} = path_match(P, leaf:path(Leaf2, CFG)),
		[H|T] = empty_stems(A-length(Branch)+1, CFG),
		LH2 = leaf:hash(Leaf2, CFG),
		H2 = stem:add(H, N2, 2, LP2, LH2),
		[H2|T]++Branch;
	    Branch -> %overwrite
		Branch
    end,
    store_branch(B, P, 2, LPointer, LH, CFG).
-type path_nibble_index() :: path_nibble_index(cfg:path()).
-type path_nibble_index(_CfgPathSizeBytes) :: non_neg_integer(). % 0..((cfg:path() * 2) - 1)
-spec get_branch(Path::leaf:path(), StartInPath::path_nibble_index(),
		 stem:stem_p(), branch(), cfg:cfg()) ->
			{leaf:leaf(), leaf:leaf_p(), % leaf (and corresponding pointer) at returned branch and containing path different from the specified one
			 Branch::nonempty_branch()} |
			nonempty_branch(). % branch either (1) without leaf or (2) with leaf containing specified path
get_branch(Path, N, Parent, Trail, CFG) ->
    %gather the branch as it currently looks.
    M = N+1,
    <<A:4>> = lists:nth(M, Path), % TODO this could be turned into hd (head)
    R = stem:get(Parent, CFG),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    ST = stem:type(A+1, R),
    if
	ST == 0 -> RP;
	Pointer == 0 -> RP;
	ST == 1 -> get_branch(Path, M, Pointer, RP, CFG);
	ST == 2 ->
	    Leaf = leaf:get(Pointer, CFG),
	    case leaf:path(Leaf, CFG) of
		Path -> %overwrite
		    RP;
		_ -> %split leaf, add stem(s)
		    {Leaf, Pointer, RP}
	    end
    end.
-spec store_branch(nonempty_branch(), leaf:path(),
		   stem:leaf_t(), leaf:leaf_p(), stem:hash(),
		   cfg:cfg()) -> Result when
      Result :: {RootHash::stem:hash(), Root::stem:stem_p(), get:proof()};
		  (nonempty_branch(), leaf:path(),
		   stem:empty_t(), stem:empty_p(), stem:hash(),
		   cfg:cfg()) -> Result when
      Result :: {RootHash::stem:hash(), Root::stem:stem_p(), get:proof()}.
store_branch(Branch = [_|_], Path, Type, Pointer, Hash, CFG) when Type =:= 0;
								  Type =:= 2 ->
    store_branch_internal(Branch, Path, Type, Pointer, Hash, CFG).
store_branch_internal([], Path, _, Pointer, _, CFG) ->
    %Instead of getting the thing, we can build it up while doing store.
    {Hash, _, Proof} = get:get(Path, Pointer, CFG),
    {Hash, Pointer, Proof};

    %case get:get(Path, Pointer, CFG) of
	%{Hash, _, Proof} -> {Hash, Pointer, Proof};
	%empty -> store_branch_internal([], Path, 0, Pointer, 0, CFG)
    %end;
store_branch_internal([B|Branch], Path, Type, Pointer, Hash, CFG) ->
    S = length(Branch),
    <<A:4>> = lists:nth(S+1,Path),  %% TODO maybe this can be turned into hd (head)
    S1 = stem:add(B, A, Type, Pointer, Hash),
    Loc = stem:put(S1, CFG),
    SH = stem:hash(S1, CFG),
    store_branch_internal(Branch, Path, 1, Loc, SH, CFG).
%add(L) -> add(L, 0).
%add([], X) -> X;
%add([H|T], X) -> add(T, H+X).
-spec path_match(Path1::leaf:path(), Path2::leaf:path()) ->
			{ConvergenceLength::path_nibble_index(),
			 NextNibbleInPath2::stem:nibble()}.
path_match(P1, P2) ->
    path_match(P1, P2, 0).
path_match([<<A:4>> | P1], [<<B:4>> | P2], N) ->
    if
	A == B -> path_match(P1, P2, N+1);
	true -> {N, B}
    end.
empty_stems(0, _) -> [];
empty_stems(N, CFG) -> [stem:new_empty(CFG)|empty_stems(N-1, CFG)].
