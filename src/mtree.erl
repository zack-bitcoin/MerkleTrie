-module(mtree).
-export([test/0]).

-record(mt, {cfg, ets, top}).
-define(ID, unnamed).

%%%%%
%% ETS Stuff
%%%%%
   
ets_new_db() ->
    ets:new(ok, [set, {write_concurrency, false}, compressed, public]).
ets_delete_db(M) ->
    ets:delete(M#mt.ets),
    ok.

ets_write_batch(M, L) ->%L is [{Top, Data}|...]
    ets:insert(M#mt.ets, L),
    Top2 = max_second(L, M#mt.top),
    M#mt{top = Top2+1}.
max_second([], X) -> X;
max_second([{L, _}|T], X) ->
    max_second(T, max(X, L)).

ets_delete(M, L) ->
    ets:delete(M#mt.ets, L),
    M.
ets_delete_all(M) ->
    ets:delete_all_objects(M#mt.ets),
    M.
ets_write(M, D) ->
    T = M#mt.top,
    ets_update(M, T, D),
    M2 = M#mt{top = T + 1},
    {M2, T}.
ets_read(M, Location) ->
    ID = M#mt.ets,
    Y = case ets:lookup(ID, Location) of
            [] -> empty;
            Z -> element(2, hd(Z))
        end,
    Y.
ets_update(M, L, D) ->
    ets:insert(M#mt.ets, [{L, D}]).

%%%%%
%%Stem/Leaf stuff
%%%%%

%Type can be `stem` or `leaf`


element_get(Type, Pointer, M) ->
    true = Pointer > 0,
    S = ets_read(M, Pointer),
    CFG = M#mt.cfg,
    Type:deserialize(S, CFG).
    
element_update(Type, Loc, Stem, M) ->
    CFG = M#mt.cfg,
    ets_update(M, Loc, Type:serialize(Stem, CFG)).
element_write(Type, Stem, M) ->
    CFG = M#mt.cfg,
    ets_write(M, Type:serialize(Stem, CFG)).
    
element_write_batch(Things, M) ->
    %SL = stem:serialize_stems(Leaves, CFG),
    SL = serialize_list(Things, M),
    ets_write_batch(M, SL).
serialize_list([], _) -> [];
serialize_list([{N, L}|T], M) ->
    Type = element(1, L),
    CFG = M#mt.cfg,
    [{N, Type:serialize(L, CFG)}|
     serialize_list(T, M)].
    
%%%%%
%% Merkle Tree Stuff
%%%%%

new(KeyLength, Size, Meta) ->
    %for governance the key length should be 8.
    %for all other trees it is 32.

    %for accounts and oracles meta is 3.
    %for all other trees it is 0.

    P = ets_new_db(),
    CFG = cfg:new(KeyLength,
	    Size,
	    ?ID,%id
	    Meta,%Meta 
	    32, %HashSize
	    ram),
    #mt{cfg = CFG, ets = P, top = 1}.


new_empty(KeyLength, Size, Meta) ->
    M = new(KeyLength, Size, Meta),
    Stem = stem:new_empty(M#mt.cfg),
    {M2, _} = element_write(stem, Stem, M),
    M2.

new_restoration(RootStem, KeyLength, Size, Meta) ->
    M = new(KeyLength, Size, Meta),
    element_write(stem, RootStem, M).

top(M) -> M#mt.top.

loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    Loc2 = F ++ "_rest.db".
save_to_file(M, Loc) ->
    Loc2 = loc2rest(Loc),
    db:save(Loc2, term_to_binary(M#mt{ets = 0})),
    ets:tab2file(M#mt.ets, Loc, [{sync, true}]).
load_from_file(Loc) ->
    Loc2 = loc2rest(Loc),
    M = db:read(loc2rest(Loc)),
    {ok, P} = ets:file2tab(Loc),
    M#mt{ets = P}.

clean(M, Pointer) ->
    {ok, ET} = ets_new_db(),
    CFG = M#mt.cfg,
    RootStem = element_get(stem, Pointer, M),
    {NewM, _} = new_restoration(RootStem, 
				cfg:path(CFG), 
				cfg:value(CFG), 
				cfg:meta(CFG)),
    {_, NewM2} = clean2(Pointer, M, NewM, CFG),
    ets_delete_db(M),
    NewM2.

clean2(Pointer, M, NewM, CFG) ->
    S = element_get(stem, Pointer, M),
    P = tuple_to_list(stem:pointers(S)),
    T = tuple_to_list(stem:types(S)),
    H = tuple_to_list(stem:hashes(S)),
    NewM2 = clean3(P, T, H, M, NewM, CFG),
    %SS = stem:serialize(S, CFG),
    element_update(stem, Pointer, S, NewM2),
    {stem:hash(S, CFG), NewM2}.
clean3([],[],_,_,M,_) -> M;
clean3([Pointer|PT],
       [Type|TT],
       [Hash|HT],
       M, NewM, CFG) -> 
    M3 = case Type of
	     0 -> %empty
		 Hash = <<0:256>>,
		 NewM;
	     1 -> %stem
		 {Hash, M2} = clean2(Pointer, M, NewM, CFG),
		 M2;
	     2 -> %leaf
		 Leaf = element_get(leaf, Pointer, M),
		 {M2, _} = element_update(leaf, Pointer, Leaf, NewM),
		 M2
	 end,
    clean3(PT, TT, HT, M, M3, CFG).

garbage(Trash, Keep, M) -> %returns {M, List}
    %trash and keep are pointers to consecutive root stems in M.
    TStem = element_get(stem, Trash, M),
    KStem = element_get(stem, Keep, M),
    M2 = if
	     Trash == Keep -> M;
	     true -> 
		 ets:delete(M, Trash)
	 end,
    garbage2(M2, TStem, KStem, 1).
garbage2(_,_,_,17) -> ok;
garbage2(M, S1, S2, N) -> 
    T1 = stem:type(N, S1),
    T2 = stem:type(N, S2),
    H1 = element(N, stem:hashes(S1)),
    H2 = element(N, stem:hashes(S2)),
    P1 = stem:pointer(N, S1),
    P2 = stem:pointer(N, S2),
    M2 = if
	     (T1 == 0) or (H1 == H2) -> 
		 M;%nothing to clean
	     true ->
		 
		 case {T1, T2} of%{trash, keep}
		     {1, 1} -> garbage(P1, P2, M);
		     {1, _} -> garbage(P1, 0, M);
		     {2, _} -> ets_delete(M, P1)
		 end
	 end,
    garbage2(M2, S1, S2, N+1).

restore(Leaf, Hash, Proof, Pointer, M) ->	    
    %Hash = root_hash(Pointer, M),
    CFG = M#mt.cfg,
    HSE = cfg:hash_size(CFG) * 8,
    true = verify:proof(Hash, Leaf, Proof, CFG),
    B1 = (leaf:value(Leaf) == empty),
    B2 = (is_binary(hd(Proof))),%A leaf is stored with the proof.
    Path = leaf:path(Leaf, CFG),
    {LPointer, LH, Type, Proof2, M3} = 
	if
	    B1 and B2 -> 
		L2 = leaf:deserialize(hd(Proof), CFG),
		{M2, LL} = element_write(leaf, L2, M),
		{LL,
		 leaf:hash(L2, CFG),
		 2,
		 tl(Proof),
		 M2};
	    B1 ->
		{0, <<0:HSE>>, 0, Proof, M};
	    not B2 ->
		{M2, P} = element_write(leaf, Leaf, M),
		{P, leaf:hash(Leaf, CFG), 2, Proof, M2}
	end,
    ReversePath = lists:reverse(store:first_n(length(Proof2), Path)), 
    Branch = proof2branch(Proof2, Type, LPointer, LH, 
			  ReversePath, M3, []),
    Branch2 = get_branch(Path, 0, Pointer, [], M3),%branch2 proves everything else.
    Branch3 = store:combine_branches(Branch, Branch2),
    store_branch(Branch3, Path, Type, LPointer, LH, M3).
    %restore2(Path, Proof, Type, LPointer, LH, M2).
store_branch([], Path, _, Pointer, _, M) ->
    %Instead of getting the thing, we can build it up while doing store.
    {Hash, _, Proof} = get(Path, Pointer, M),
    {Hash, Pointer, Proof, M};
store_branch([B|Branch], Path, Type, Pointer, Hash, M) ->
    CFG = M#mt.cfg,
    S = length(Branch),
    <<A:4>> = lists:nth(S+1,Path),
    S1 = stem:add(B, A, Type, Pointer, Hash),
    {M2, Loc} = element_write(stem, S1, M),
    SH = stem:hash(S1, CFG),
    store_branch(Branch, Path, 1, Loc, SH, M2).

get_branch(Path, N, Parent, Trail, Mt) ->
    %gather the branch as it currently looks.
    CFG = Mt#mt.cfg,
    M = N+1,
    <<A:4>> = lists:nth(M, Path), 
    %R = stem:get(Parent, CFG),
    R = element_get(stem, Parent, Mt),
    Pointer = stem:pointer(A+1, R),
    RP = [R|Trail],
    ST = stem:type(A+1, R),
    if
	ST == 0 -> RP;
	Pointer == 0 -> RP;
	ST == 1 -> get_branch(Path, M, Pointer, RP, CFG);
	ST == 2 ->
	    %Leaf = leaf:get(Pointer, CFG),
	    Leaf = element_get(leaf, Pointer, Mt),
	    case leaf:path(Leaf, CFG) of
		Path -> %overwrite
		    RP;
		_ -> %split leaf, add stem(s)
		    {Leaf, Pointer, RP}
	    end
    end.
proof2branch([],_,_,_,_,M,R) -> 
    {M, lists:reverse(R)};
proof2branch([H|T], _, _, Hash, _, M, R) when is_binary(H) -> 
    CFG = M#mt.cfg, 
    L = leaf:deserialize(H, CFG),
    Path = leaf:path(L, CFG),
    %Pointer = leaf:put(L, CFG),
    {M2, Pointer} = element_write(leaf, L, M),
    proof2branch(T, 2, Pointer, Hash, Path, M2, R);
proof2branch([H|T], Type, Pointer, Hash, Path, M,R) -> 
    CFG = M#mt.cfg, 
    [<<Nibble:4>> | NewPath] = Path,
    S = stem:recover(Nibble, Type, Pointer, Hash, H, CFG),
    %NewPointer = stem:put(S, CFG),
    {M2, NewPointer} = element_write(stem, S, M),
    NewHash = stem:hash(S, CFG),
    proof2branch(T, 1, NewPointer, NewHash, NewPath, M, [S|R]).
%[S|proof2branch(T, 1, NewPointer, NewHash, NewPath, CFG)].

store_batch(Leaves, Root, M) ->
    CFG = M#mt.cfg,
    %Leaves2 = store:sort_by_path(Leaves, CFG),
    Leaves2 = lists:sort(fun(A, B) ->
				 KeyA = leaf:path_maker(leaf:key(A), CFG),
				 KeyB = leaf:path_maker(leaf:key(B), CFG),
				 store:compare_keys(KeyB, KeyA) end, Leaves),
    Top = M#mt.top,
    {BranchData0, ToStore} = store_batch_helper_ram(Leaves2, M, [], Root, Top, []),
    M2 = element_write_batch(ToStore, M),%write leaves.
    BranchData = store:extra_stem(BranchData0, CFG),
    BStart = store:max_list(
	       lists:map(
		 fun(Branch) ->
			 {_,_,_,B,_} = Branch,
			 length(B)
		 end, BranchData)),
    StemTop = M2#mt.top,
    {FHash, FLoc, SToStore} = store:batch2_ram(BStart, BranchData, CFG, StemTop, []),
    F = fun({P, _}) -> P end,
    io:fwrite("top: "),
    io:fwrite(integer_to_list(Top)),
    io:fwrite("\nstemtop: "),
    io:fwrite(integer_to_list(StemTop)),
    io:fwrite("\nhd(tostore): "),
    io:fwrite(integer_to_list(F(hd(ToStore)))),
    io:fwrite("\nlength(tostore): "),
    io:fwrite(integer_to_list(length(ToStore))),
    io:fwrite("\nhd(stemstore): "),
    io:fwrite(integer_to_list(F(hd(SToStore)))),
    io:fwrite("\n"),
    %io:fwrite({Top, StemTop,
	%       lists:map(F, ToStore), 
	%       lists:map(F, SToStore)}),
    M3 = element_write_batch(SToStore, M2),%write leaves.
    {FLoc, M3}.
store_batch_helper_ram([], _, X, _, _Pointer, L) ->
    {X, L};
store_batch_helper_ram([H|T], M, BD, Root, Pointer, L) ->
    CFG = M#mt.cfg,
    Path = leaf:path(H, CFG),
    GB = get_branch(Path, 0, Root, [], M),
    case leaf:value(H) of
       empty ->
	       case GB of
		  {_, _, _} -> store_batch_helper_ram(T, M, BD, Root, Pointer, L); %if you are deleting something that doesn't exist, then you don't have to do anything.
		  Branch0 ->
		          X = cfg:hash_size(CFG)*8,
		          store_batch_helper_ram(T, M, [{0, <<0:X>>, Path, Branch0, 0}|BD], Root, Pointer, L)
			      end;
       _ ->
	       %NLP = leaf:put(H, CFG),
	       NLH = leaf:hash(H, CFG),
	       {Br, NewPointer, L2} = 
	       case GB of
		      {Leaf2, _LP1, Branch} ->%split leaf, add stem(s)
		      %LP2 = leaf:put(Leaf2, CFG),
		      %need to add 1 or more stems.
		       {A, N2} = store:path_match(Path, leaf:path(Leaf2, CFG)),
		       [Hp|Tp] = store:empty_stems(max(1, A-length(Branch)+1), CFG),
		       LH2 = leaf:hash(Leaf2, CFG),
		       H2 = stem:add(Hp, N2, 2, Pointer + 1, LH2),
		       {[H2|Tp]++Branch, Pointer + 2, [{Pointer + 1, Leaf2}|[{Pointer, H}|L]]};
		       AB -> %overwrite
		       {AB, Pointer + 1, [{Pointer, H}|L]}
			   end,
	    store_batch_helper_ram(T, M, [{Pointer, NLH, Path, Br, 2}|BD], Root, NewPointer, L2)
    end.


get(Path, Root, M) ->
    CFG = M#mt.cfg,
    S = element_get(stem, Root, M),
    H = stem:hash(S, CFG),
    case get2(Path, S, [stem:hashes(S)], M) of
	{unknown, Proof} -> {H, unknown, Proof};
	{empty, Proof} -> {H, empty, Proof};
	{A, Proof} -> {H, A, Proof}
    end.       
get2([<<N:4>> | Path], Stem, Proof, M) ->
    CFG = M#mt.cfg,
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    if
	NextType == 0 -> %empty
	    %Next = stem:get(PN, CFG),
	    {empty, Proof};
	PN == 0 -> {unknown, Proof};
	NextType == 1 -> %another stem
	    Next = element_get(stem, PN, M),
	    get2(Path, Next, [stem:hashes(Next)|Proof], CFG);
	NextType == 2 -> %leaf
	    Leaf2 = element_get(leaf, PN, M),
	    LPath = leaf:path(Leaf2, CFG),
	    B = get:same_end(LPath, Path, CFG),
	    LV = leaf:key(Leaf2),
	    if
		B -> {Leaf2, Proof};
		LV == 0 -> 
		    {empty, Proof};
		true -> 
		    {empty, [leaf:serialize(Leaf2, CFG)|Proof]}
	    end
    end.

get_all(Pointer, M) ->
    %S = stem:get(Pointer, CFG),
    %S = ets_read(M, RootPointer),
    S = element_get(stem, Pointer, M),
    P = tuple_to_list(stem:pointers(S)),
    T = tuple_to_list(stem:types(S)),
    CFG = M#mt.cfg,
    get_all_internal2(P, T, M).
get_all_internal2([], [], _) -> [];
get_all_internal2([A|AT], [T|TT], M) -> 
    B = case T of
	    0 -> [];%empty
	    1 -> get_all(A, M);%another
	    2 -> [element_get(leaf, A, M)]%leaf:get(A, CFG)]%a leaf
	end,
    B++get_all_internal2(AT, TT, M).

root_hash(Pointer, M) ->
    CFG = M#mt.cfg,
    S = ets_read(M, Pointer),
    stem:hash(S, CFG).


test() ->
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    Meta = 0,
    M = new_empty(5, 2, 0),
    CFG = M#mt.cfg,
    Leaf1 = leaf:new(1, V1, Meta, CFG),
    Leaf2 = leaf:new(2, V2, Meta, CFG),
    %RH0 = root_hash(0, M),
    Leaves = [Leaf1, Leaf2],
    {Root, M2} = store_batch(Leaves, 1, M),
    Path = leaf:path(Leaf1, CFG),
    Path2 = leaf:path(Leaf2, CFG),
    {Hash1, Leaf1, Proof} = get(Path, Root, M2),
    {Hash1, Leaf2, Proof} = get(Path2, Root, M2),
    %X = get_all(Root, M2),
    true = verify:proof(Hash1, Leaf1, Proof, CFG),
    %RootStem = ets_read(M2, Root),
    RootStem = element_get(stem, Root, M2),
    %io:fwrite(RootStem),
    {M3, Root3} = new_restoration(
	   %stem:deserialize(RootStem, CFG), 
		     RootStem,
		     5, 2, 0),
    %io:fwrite(Root3),
    true = verify:proof(Hash1, Leaf1, Proof, CFG),
    {Hash1, _, Proof, M4} = restore(Leaf1, Hash1, Proof, Root3, M3),
    {Hash1, Leaf1, Proof} = get(Path, Root3, M4),

    Leaf3 = leaf:new(3, V3, Meta, CFG),
    {Root2, M5} = store_batch([Leaf3], Root, M2),
    %[Path, leaf:path(Leaf3, CFG)].
    
    %garbage(Root, Root2, M4),
    %M4.
    get(leaf:path(Leaf2, CFG), Root2, M5).
    %ok.
    %TODO
    %test garbage
    %passing test of get_all
    %saving to file, and loading from file
    %clean
    %success.

    %ets:insert(M#mt.ets, {
%ok.
