-module(garbage).
-export([garbage/1, garbage_leaves/1]).
garbage_leaves(KeeperLeaves) ->
    KeeperStems = keepers_backwards(KeeperLeaves),
    delete_stuff(0, KeeperStems, stem),
    delete_stuff(0, KeeperLeaves, leaf),
    ok.
garbage(KeeperRoots) ->
    {KeeperStems, KeeperLeaves} = keepers(KeeperRoots),
    delete_stuff(0, KeeperStems, stem),
    delete_stuff(0, KeeperLeaves, leaf),
    ok.
keepers_backwards(X) -> keepers_backwards(X, []).
keepers_backwards([], X) -> X;
keepers_backwards([{Path, Root}|Leaves], X) -> 
    S = stem:deserialize(dump:get(Root, stem)),
    Keepers = kb2(Path, S, [Root]),
    keepers_backwards(Leaves, X++Keepers).
kb2(<<N:4, Path/bitstring>>, Stem, Keepers) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	1 -> %another stem
	    Next = stem:deserialize(dump:get(PN, stem)), 
	    kb2(Path, Next, [PN|Keepers]);
	2 -> %leaf
	    Keepers
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
append_no_repeats(X, Y) -> append_no_repeats(X, Y, []).
append_no_repeats([],[],X) -> X;
append_no_repeats([],B,X) -> 
    append_no_repeats(B,[],X);
append_no_repeats([A|Ta],B,X) -> 
    Bool1 = in_list(A, B),
    Bool2 = in_list(A, X),
    if
	Bool1 -> append_no_repeats(Ta, B, X);
	Bool2 -> append_no_repeats(Ta, B, X);
	true -> append_no_repeats(Ta,B,[A|X])
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
    S = dump:highest(Id),
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
