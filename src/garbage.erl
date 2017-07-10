-module(garbage).
-export([garbage/2, garbage_leaves/2]).
garbage_leaves(KeeperLeaves, CFG) ->
    {KeeperStems, KL} = keepers_backwards(KeeperLeaves, CFG),
    dump_bits(KL, CFG),
    delete_stuff(0, KL, ids:leaf(CFG)),
    delete_stuff(0, [0|KeeperStems], ids:stem(CFG)),
    ok.
-spec garbage([stem:stem_p()], cfg:cfg()) -> ok.
garbage(KeeperRoots, CFG) ->
    {KeeperStems, KeeperLeaves} = keepers(KeeperRoots, CFG),
    %io:fwrite(integer_to_list(length(KeeperLeaves))),
    delete_stuff(0, KeeperStems, ids:stem(CFG)),
    delete_stuff(0, KeeperLeaves, ids:leaf(CFG)),
    dump_bits(KeeperLeaves, CFG),
    ok.
dump_bits([], _) -> ok;
dump_bits([K|T], CFG) -> 
    Leaf = leaf:get(K, CFG),
    Path = leaf:path(Leaf, CFG),
    NN = cfg:path(CFG)*8,
    <<P:NN>> = Path,
    bits:delete(ids:bits(CFG), P),
    dump_bits(T, CFG).
keepers_backwards(X, CFG) -> keepers_backwards(X, {[],[]}, CFG).
keepers_backwards([], X, _) -> X;
keepers_backwards([{Path, Root}|Leaves], {KS, KL}, CFG) -> 
    S = stem:get(Root, CFG),
    {Stems, Leaf} = kb2(Path, S, [Root], CFG),
    keepers_backwards(Leaves, 
		      {append_no_repeats(KS, Stems), 
		       append_no_repeats([Leaf], KL)},
		      CFG).
kb2(<<N:4, Path/bitstring>>, Stem, Keepers, CFG) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	1 -> %another stem
	    Next = stem:get(PN, CFG),
	    kb2(Path, Next, append_no_repeats([PN], Keepers), CFG);
	2 -> %leaf
	    {Keepers, PN}
    end.
keepers([], _) -> {[], []};
keepers([R|Roots], CFG) -> %returns {keeperstems, keeperleaves}
    case stem:get(R, CFG) of
	error -> 
	    {A, B} = keepers(Roots, CFG),
	    {[R|A],B};
	S -> 
	    {X, Y, MoreRoots} = stem_keepers(S),
	    {A, B} = keepers(MoreRoots++Roots, CFG),
	    {[R|append_no_repeats(X, A)],
	     append_no_repeats(Y, B)}
    end.
append_no_repeats([],X) -> X;
append_no_repeats([A|Ta],X) -> 
    Bool2 = in_list(A, X),
    if
	Bool2 -> append_no_repeats(Ta, X);
	true -> append_no_repeats(Ta, [A|X])
    end.
stem_keepers(S) ->
    stem_keepers(S, 1, [], [], []).
stem_keepers(_, 17, Stems, Leaves, Roots) -> {Stems,Leaves, Roots};
stem_keepers(S, N, Stems, Leaves, MoreRoots) ->
    P = stem:pointer(N, S),
    {NewStems, NewLeaves, NewMoreRoots} = 
	case stem:type(N, S) of
	    0 -> {Stems, Leaves, MoreRoots};
	    1 -> 
		{[P|Stems], Leaves,[P|MoreRoots]};
	    2 -> {Stems, [P|Leaves], MoreRoots}
	end,
    stem_keepers(S, N+1, NewStems, NewLeaves, NewMoreRoots).
in_list(X, [X|_]) -> true;
in_list(_, []) -> false;
in_list(X, [_|Z]) -> in_list(X, Z).
delete_stuff(N, Keepers, ID) ->
    S = dump:highest(ID) div dump:word(ID),
    delete_stuff(S, N, Keepers, ID).
delete_stuff(S, N, Keepers, Id) ->
    Bool = in_list(N, Keepers),
    if
	N>=S -> ok;
	Bool ->
	    delete_stuff(S, N+1, Keepers, Id);
	true ->
	    dump:delete(N, Id),
	    delete_stuff(S, N+1, Keepers, Id)
    end.
