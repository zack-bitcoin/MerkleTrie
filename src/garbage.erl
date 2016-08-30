-module(garbage).
-export([garbage/1, garbage_leaves/1]).
garbage_leaves(KeeperLeaves) ->
    {KeeperStems, KL} = keepers_backwards(KeeperLeaves),
    delete_stuff(0, KL, leaf),
    delete_stuff(0, KeeperStems, stem),
    ok.
garbage(KeeperRoots) ->
    {KeeperStems, KeeperLeaves} = keepers(KeeperRoots),
    io:fwrite("KeeperLeaves"),
    io:fwrite(integer_to_list(length(KeeperLeaves))),
    delete_stuff(0, KeeperStems, stem),
    delete_stuff(0, KeeperLeaves, leaf),
    ok.
keepers_backwards(X) -> keepers_backwards(X, {[],[]}).
keepers_backwards([], X) -> X;
keepers_backwards([{Path, Root}|Leaves], {KS, KL}) -> 
    S = stem:deserialize(dump:get(Root, stem)),
    {Stems, Leaf} = kb2(Path, S, [Root]),
    keepers_backwards(Leaves, {append_no_repeats(KS, Stems), 
			       append_no_repeats([Leaf], KL)}).
kb2(<<N:4, Path/bitstring>>, Stem, Keepers) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	1 -> %another stem
	    Next = stem:deserialize(dump:get(PN, stem)), 
	    kb2(Path, Next, append_no_repeats([PN], Keepers));
	2 -> %leaf
	    {Keepers, PN}
    end.
    

keepers([]) -> {[], []};
keepers([R|Roots]) ->
    case stem:deserialize(dump:get(R, stem)) of
	error -> 
	    {A, B} = keepers(Roots),
	    {[R|A],B};
	S -> 
	    {X, Y, MoreRoots} = stem_keepers(S),
	    {A, B} = keepers(MoreRoots++Roots),
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
delete_stuff(N, Keepers, Id) ->
    S = dump:highest(Id) div dump:word(Id),
    delete_stuff(S, N, Keepers, Id).
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
