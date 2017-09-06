%choices on how to fix garbage_leaves
%1 we could cycle through every stem in the trie, check every pointer, and see if it is deleted.
%2 we could rethink how garbage and garbage_leaves work, so keepers is combined with delete_stuff. Instead of cycling through every stem, we walk down the 

-module(garbage).
-export([garbage/2, garbage_leaves/2]).
garbage_leaves(KeeperLeaves, CFG) ->
    {KeeperStems, KL} = keepers_backwards(KeeperLeaves, CFG),
    %We need to update all the Keeper stems, so that the empty things aren't pointed to.
    DeletedLeaves = delete_stuff(1, KL, ids:leaf(CFG)),
    DeletedStems = delete_stuff(1, [1|KeeperStems], ids:stem(CFG)),%maybe delete_stuff should return the locations of all deleted stems and leaves, that way we know what to replace with 0s.
    remove_bad_pointers(1, [1|KeeperStems], DeletedStems, CFG),%1 is for stems
    remove_bad_pointers(2, [1|KeeperStems], DeletedLeaves, CFG),%2 is for leaves
    ok.
remove_bad_pointers(_, [], _, _) -> ok;
remove_bad_pointers(PT, [K|KT], DS, CFG) ->
    Stem = stem:get(K, CFG),
    Pointers = stem:pointers(Stem),
    Types = stem:types(Stem),
    NewPointers = rbp2(PT, Pointers, Types, DS),
    Stem2 = stem:update_pointers(Stem, NewPointers),
    stem:update(K, Stem2, CFG),
    remove_bad_pointers(PT, KT, DS, CFG).
rbp2(PT, Pointers, Types, DS) ->
    X = rbp3(tuple_to_list(Pointers),
	      tuple_to_list(Types),
	      DS, PT),
    list_to_tuple(X).
rbp3([], [], _, _) -> [];
rbp3([P|PT], [T|TT], DS, PointerType) ->
    B = (lists:member(P, DS)) and (T == PointerType),
    case B of
	true -> 
	    [0|rbp3(PT, TT, DS, PointerType)];
	false -> [P|rbp3(PT, TT, DS, PointerType)]
    end.
	    
-spec garbage([stem:stem_p()], cfg:cfg()) -> ok.
garbage(KeeperRoots, CFG) ->
    {KeeperStems, KeeperLeaves} = keepers(KeeperRoots, CFG),
    delete_stuff(1, KeeperStems, ids:stem(CFG)),
    delete_stuff(1, KeeperLeaves, ids:leaf(CFG)),
    ok.
keepers_backwards(X, CFG) -> keepers_backwards(X, {[],[]}, CFG).
keepers_backwards([], X, _) -> X;
keepers_backwards([{Path, Root}|Leaves], {KS, KL}, CFG) -> 
    S = stem:get(Root, CFG),
    {Stems, Leaf} = kb2(Path, S, [Root], CFG),
    keepers_backwards(Leaves, 
		      {append_no_repeats(Stems, KS), 
		       append_no_repeats([Leaf], KL)},
		      CFG).
kb2([<<N:4>> | Path], Stem, Keepers, CFG) ->
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
    Bool2 = lists:member(A, X),
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
delete_stuff(_, Keepers, ID) ->
    S = dump:highest(ID) div dump:word(ID),
    delete_stuff(S, 1, Keepers, ID, []).
delete_stuff(S, N, Keepers, Id, Out) ->
    Bool = lists:member(N, Keepers),
    if
	N>S -> Out;%we should go through the list of keepers and update any pointers that point to deleted data to instead point to 0.
	Bool ->
	    delete_stuff(S, N+1, Keepers, Id, Out);
	true ->
	    dump:delete(N, Id),
	    delete_stuff(S, N+1, Keepers, Id, [N|Out])
    end.
