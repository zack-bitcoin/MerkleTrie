-module(garbage).
-export([garbage/5, garbage_leaves/5]).
garbage_leaves(KeeperLeaves, M, ID, WS, LS) ->
    {KeeperStems, KL} = keepers_backwards(KeeperLeaves, ID, WS),
    dump_bits(KL, M, ID, WS, LS),
    delete_stuff(0, KL, ids:leaf(ID)),
    delete_stuff(0, KeeperStems, ids:stem(ID)),
    ok.
garbage(KeeperRoots, M, ID, WS, LS) ->
    {KeeperStems, KeeperLeaves} = keepers(KeeperRoots, ID, WS),
    io:fwrite(integer_to_list(length(KeeperLeaves))),
    delete_stuff(0, KeeperStems, ids:stem(ID)),
    delete_stuff(0, KeeperLeaves, ids:leaf(ID)),
    dump_bits(KeeperLeaves, M, ID, WS, LS),
    ok.
dump_bits([], _, _, _, _) -> ok;
dump_bits([K|T], M, ID, WS, LS) -> 
    Leaf = leaf:get(K, WS, LS, ID),
    Path = leaf:path(Leaf, M),
    NN = M*8,
    <<P:NN>> = Path,
    dump_bits:delete(ids:bits(ID), P),
    dump_bits(T, M, ID, WS, LS).
keepers_backwards(X, ID, WS) -> keepers_backwards(X, {[],[]}, ID, WS).
keepers_backwards([], X, _, _) -> X;
keepers_backwards([{Path, Root}|Leaves], {KS, KL}, ID, WS) -> 
    S = stem:get(Root, WS, ID),
    {Stems, Leaf} = kb2(Path, S, [Root], ID, WS),
    keepers_backwards(Leaves, 
		      {append_no_repeats(KS, Stems), 
		       append_no_repeats([Leaf], KL)}, 
		      ID, 
		      WS).
kb2(<<N:4, Path/bitstring>>, Stem, Keepers, ID, WS) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	1 -> %another stem
	    Next = stem:get(PN, WS, ID),
	    kb2(Path, Next, append_no_repeats([PN], Keepers), ID, WS);
	2 -> %leaf
	    {Keepers, PN}
    end.
    

keepers([], _, _) -> {[], []};
keepers([R|Roots], ID, WS) ->
    case stem:get(R, WS, ID) of
	error -> 
	    {A, B} = keepers(Roots, ID, WS),
	    {[R|A],B};
	S -> 
	    {X, Y, MoreRoots} = stem_keepers(S),
	    {A, B} = keepers(MoreRoots++Roots, ID, WS),
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
