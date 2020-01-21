-module(test_trie).
-export([test/0, test/2]).

-define(ID, trie01).

test() ->
    CFG = trie:cfg(?ID),
    %V = [1,2,3,4,5,7,8,9,10,11,12,13,14,16,17],
    V = [1,2,3,5,7,8,9,10,11,12,13,14,16,17,18],
    %V = [5, 6, 12, 13],
    %V = [18],
    test_helper(V, CFG).
test_helper([], _) -> success;
test_helper([N|T], CFG) -> 
    io:fwrite("test "),
    io:fwrite(integer_to_list(N)),
    io:fwrite("\n"),
    success = test(N, CFG),
    test_helper(T, CFG).

test(1, CFG) ->
    leaf:new(1, empty, 0, CFG),
    Nib1 = 4,
    Nib2 = 2,
    L = [<<Nib1:4>>,<<Nib2:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>],
    Lflip = lists:reverse(L),
    Lb = <<255,255>>,
    <<Laa:40>> = list_to_bitstring(Lflip),
    Meta = 0,
    LeafAB = leaf:new(Laa, Lb, Meta, CFG), 
    %io:fwrite("leafAB is "),
    %io:fwrite(LeafAB),
        %called as io:format(<0.26.0>,{leaf,36,<<"\377\377">>,0},[])
    Lc = leaf:serialize(LeafAB, CFG),
    %io:fwrite("lc is "),
    %io:fwrite(size(Lc)),
    Loc1 = dump:put(Lc, ids:leaf(CFG)),
    LH = leaf:hash(LeafAB, CFG),
    S1 = stem:new(Nib2, 2, Loc1, LH, CFG),
    Loc2 = stem:put(S1, CFG),
    SH = stem:hash(S1, CFG),
    S = stem:new(Nib1, 1, Loc2, SH, CFG),
    Loc3 = stem:put(S, CFG),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S, CFG),
    X = {RootHash, LeafAB, [stem:hashes(S1), stem:hashes(S)]},
    Proof = [stem:hashes(S1), stem:hashes(S)], 
    {RootHash, LeafAB, _} = get:get(L, Loc3, CFG),%Path, Root
    X = get:get(L, Loc3, CFG),%Path, Root
    {_, LeafAB, Proof} = X,
    true = verify:proof(RootHash, LeafAB, Proof, CFG),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = [<<Nib3:4>>,<<Nib4:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>],
    L2flip = lists:reverse(L2),
    L2b = <<255,255>>,
    <<Lbb:40>> = list_to_bitstring(L2flip),
    Leafbb = leaf:new(Lbb, L2b, Meta, CFG),
    L2c = leaf:serialize(Leafbb, CFG),
    Loc4 = dump:put(L2c, ids:leaf(CFG)),
    LH2 = leaf:hash(Leafbb, CFG),
    S2 = stem:new(Nib4, 2, Loc4, LH2, CFG),
    Loc5 = stem:put(S2, CFG),
    SH2 = stem:hash(S2, CFG),
    S3 = stem:add(S, Nib3, 1, Loc5, SH2),
    Loc6 = stem:put(S3, CFG),
    RootHash2 = stem:hash(S3, CFG),
    Proof2 = [stem:hashes(S2), stem:hashes(S3)],
    X2 = {RootHash2, Leafbb, Proof2},
    X2 = get:get(L2, Loc6, CFG),
    true = verify:proof(RootHash2, Leafbb, Proof2, CFG),
    Nib5 = 4,
    Nib6 = 2,
    L3 = [<<Nib5:4>>,<<Nib6:4>>,<<0:4>>,<<1:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>,<<0:4>>],
    L3flip = lists:reverse(L3),
    L3b = <<255,255>>,
    <<L3abc:40>> = list_to_bitstring(L3flip),
    Leafcc = leaf:new(L3abc, L3b, Meta, CFG), 
    {Root7, Loc7, _} = store:store(Leafcc, Loc6, CFG),
    {Root7, _, _} = store:store(Leafcc, Loc6, CFG),
    %trie:garbage([Loc7], ?ID),
    %timer:sleep(100),
    trie:cfg(?ID),
    ReplaceStem = <<0:(8*(dump:word(ids:stem(CFG))))>>,
    %1 = dump:put(ReplaceStem, ids:stem(CFG)),
    {PP4,Leafcc,PP5} = get:get(L3, Loc7, CFG),
    true = verify:proof(PP4,Leafcc,PP5,CFG),
    success;

test(2, CFG) ->
    Loc = 1,
    %L = <<0:4,0:4,0:4,0:4,0,0,0>>,
    La = <<255, 0>>,
    Leaf = leaf:new(1, La, 0, CFG),
    store:store(Leaf, Loc, CFG),
    success;

test(3, CFG) -> 
    Loc = 1,
    Times = 1000,
    NewLoc = test3a(Times, Times, Loc),
    test3b(Times, NewLoc, CFG),
    success;

test(4, CFG) ->
    Size = dump:word(ids:leaf(CFG)),
    Size1 = cfg:leaf(CFG),
    %io:fwrite({Size, Size1}),
    %Size = Size1,
    Data0 = <<11:(8*Size)>>,
    Data1 = <<2:(8*Size)>>,
    Data2 = <<3:(8*Size)>>,
    Data3 = <<4:(8*Size)>>,
    IDSL = ids:leaf(CFG),
    IDSS = ids:stem(CFG),
    A0 = dump:put(Data0, IDSL),
    Data0 = dump:get(A0, IDSL),
    A1 = dump:put(Data1, IDSL),
    Data1 = dump:get(A1, IDSL),
    dump:delete(A0, IDSL),
    A0 = dump:put(Data1, IDSL),
    Data1 = dump:get(A0, IDSL),
    A2 = dump:put(Data2, IDSL),
    Data1 = dump:get(A0, IDSL),
    A3 = dump:put(Data3, IDSL),
    Data1 = dump:get(A0, IDSL),
    Data1 = dump:get(A1, IDSL),
    Data2 = dump:get(A2, IDSL),
    Data3 = dump:get(A3, IDSL),

    Size2 = dump:word(IDSS),
    Data02 = <<11:(8*Size2)>>,
    Data12 = <<2:(8*Size2)>>,
    Data22 = <<3:(8*Size2)>>,
    Data32 = <<4:(8*Size2)>>,
    A02 = dump:put(Data02, IDSS),
    A12 = dump:put(Data12, IDSS),
    A22 = dump:put(Data22, IDSS),
    A32 = dump:put(Data32, IDSS),
    Data02 = dump:get(A02, IDSS),
    Data12 = dump:get(A12, IDSS),
    Data22 = dump:get(A22, IDSS),
    Data32 = dump:get(A32, IDSS),
    dump:delete(A02, IDSS),
    success;

test(5, CFG) ->
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    <<L1:40>> = <<1,0,0,0,0>>,
    <<L2:40>> = <<0, 0,16,0,0>>,
    <<L3:40>> = <<0, 0,1,0,0>>,
    Meta = 0,
    Leaf1 = leaf:new(L1, V1, Meta, CFG),
    Leaf2 = leaf:new(L2, V2, Meta, CFG),
    Leaf3 = leaf:new(L3, V3, Meta, CFG),
    Leaf4 = leaf:new(L3, V1, Meta, CFG),
    {_, Root1, _} = store:store(Leaf1, Root0, CFG),
    {Hash, Root2, Proof2} = store:store(Leaf2, Root1, CFG),
    {Hash, Root3, Proof3} = store:store(Leaf2, Root2, CFG),
    {Hash2, Root4, Proof4} = store:store(Leaf3, Root3, CFG),
    {Hash2, Leaf2, Proof7} = get:get(leaf:path(Leaf2, CFG), Root4, CFG),
    {Hash2, Leaf3, Proof8} = get:get(leaf:path(Leaf3, CFG), Root4, CFG),
    Lpath1 = leaf:path(Leaf1, CFG),
    X = [{Lpath1, Root4}],
    %garbage:garbage_leaves(X, CFG),%After we do garbage leaves we can't insert things into the merkle tree normally. 
    %many stems are missing, so we can't make proofs of anything we don't save, but we can still verify them.
    %We need a merkle proof of it's previous state in order to update.
    %timer:sleep(500),
    %timer:sleep(500),
    {Hash2, Leaf1, Proof} = get:get(Lpath1, Root4, CFG),
    true = verify:proof(Hash2, Leaf1, Proof, CFG),
    true = verify:proof(Hash, Leaf2, Proof2, CFG),
    %{Hash2, unknown, _} = get:get(leaf:path(Leaf2, CFG), Root4, CFG),
    {Hash2, Root5, _} = store:restore(Leaf2, Hash2, Proof7, Root4, CFG),
    %{Hash2, unknown, _} = get:get(leaf:path(Leaf3, CFG), Root5, CFG),
    {Hash2, Root6, Proof5} = store:restore(Leaf3, Hash2, Proof8, Root5, CFG),
    %we need to be able to add proofs for things into an empty database.
    true = verify:proof(Hash2, Leaf3, Proof5, CFG),
    {Hash5, _, Proof6} = store:store(Leaf4, Root6, CFG), %overwrite the same spot.
    true = verify:proof(Hash5, Leaf4, Proof6, CFG),
    success;

test(6, CFG) ->
    %The purpose of this test is to test merge.
    % The full merkel trie will be too big, most people wont keep track of it all. 
    % sometimes parts of the trie get updated that we aren't keeping track of. We need to look at the proof of their update, and update our state root accordingly.
    % We don't get a proof of the final state. We only get a proof of the initial state, and the final state. It is possible to calculate the new proof from this. The reason we don't get the new proof is because depending on which txs get accepted into the block, the root hash of the new state will be different
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    <<L1:40>> = <<0,0,0,0,1>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    Meta = 0,
    Leafa = leaf:new(L1, V1, Meta, CFG),
    {_, Root1, _} = store:store(Leafa, Root0, CFG),
    {Hash0bb, Leafa, Proofa} = get:get(leaf:path(Leafa, CFG), Root1, CFG),
    true = verify:proof(Hash0bb, Leafa, Proofa, CFG),
    Leafb = leaf:new(L2, V2, Meta, CFG),
    {_, Root2, _} = store:store(Leafb, Root1, CFG),
    {Hash0, Leafb, Proofb} = get:get(leaf:path(Leafb, CFG), Root2, CFG),
    true = verify:proof(Hash0, Leafb, Proofb, CFG),
    Leafc = leaf:new(L2, V3, Meta, CFG),
    {Hash, Root3, _} = store:store(Leafc, Root2, CFG),
    {Hasha, _, _} = store:store(Leafc, Root1, CFG),
    Hasha = Hash,
    {Hash, Leafc, Proofc} = get:get(leaf:path(Leafc, CFG), Root3, CFG),
    true = verify:proof(Hash, Leafc, Proofc, CFG),
    {Hash, Root6, Proofc} = store:store(Leafc, Root1, CFG),
    GL = [{leaf:path(Leafa, CFG), Root6}],
    {_, Leafa, _} = get:get(leaf:path(Leafa, CFG), Root6, CFG),
    garbage:garbage_leaves(GL, CFG),
    timer:sleep(1000),
    {Hash3, Leafa, _} = get:get(leaf:path(Leafa, CFG), Root6, CFG),
    %{Hash3, empty, _} = get:get(leaf:path(Leafc, CFG), Root6, CFG),
    %RootStem = stem:update_pointers(stem:get(Root6, CFG),
	%			    stem:empty_tuple()),
    %Root7 = trie:new_trie(trie01, RootStem),
    %RootStem = stem:empty_trie(stem:get(Root6, CFG), CFG),
    Root7 = trie:new_trie(trie01, stem:get(Root6, CFG)),
    Hash3 = trie:root_hash(trie01, Root7),
    {Hash3, unknown, _} = get:get(leaf:path(Leafc, CFG), Root7, CFG),
    {Hash3, Root8, _} = store:restore(Leafc, Hash, Proofc, Root7, CFG), %it is restoring the deleted leaf to the database.
    %{Hash, Leafa, _B2} = get:get(leaf:path(Leafa, CFG), Root5, CFG),
    {Hash3, Leafc, _} = get:get(leaf:path(Leafc, CFG), Root8, CFG),
    %true = verify:proof(Hash, Leafa, B2, CFG),


% the current implementation is very innefficient. It stores the entire proof onto the hard drive
    success;

test(7, CFG) ->
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    <<L1:40>> = <<0,0,0,0,2>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    Meta = 0,
    Leaf1 = leaf:new(L1, V1, Meta, CFG),
    Leaf2 = leaf:new(L2, V2, Meta, CFG),
    {_, Root1, _} = store:store(Leaf1, Root0, CFG),
    {Hash0bb, Leaf1, B0bb} = get:get(leaf:path(Leaf1, CFG), Root1, CFG),
    true = verify:proof(Hash0bb, Leaf1, B0bb, CFG),
    {_, Root2, _} = store:store(Leaf2, Root1, CFG),
    {Hash0, Leaf2, B0} = get:get(leaf:path(Leaf2, CFG), Root2, CFG),
    true = verify:proof(Hash0, Leaf2, B0, CFG),
    success;
    
test(8, CFG) ->    
    V1 = <<1,1>>,
    Root = 1,
    Key = 1,
    Meta = 0, 
    Root2 = trie:put(Key, V1, Meta, Root, trie01),
    {RootHash, empty, Proof} = trie:get(2, Root2, trie01),
    {RootHash, empty, _} = trie:get(3, Root2, trie01),
    {_, empty, _} = trie:get(4, Root2, trie01),
    {_, Leaf, _} = trie:get(Key, Root2, trie01),
    V1 = leaf:value(Leaf),
    true = verify:proof(RootHash, leaf:new(2, empty, 0, CFG), 
			Proof, CFG),
    success;
    
test(9, CFG) ->
    %Root0 = 1,
    %io:fwrite(CFG),
    Root0 = cfg:empty(CFG),
    S = stem:get(Root0, CFG),
    V1 = <<2,3>>,
    Key = 5,
    RH = trie:root_hash(trie01, Root0),
    Meta = 0, 
    Root2 = trie:put(Key, V1, Meta, Root0, trie01),
    {_, Leaf, _Proof1} = trie:get(Key, Root2, trie01),
    V1 = leaf:value(Leaf),
    Root3 = trie:delete(Key, Root2, trie01),
    {RH, empty, _Proof} = trie:get(Key, Root3, trie01),
    S = stem:get(Root0, CFG),
    success;
    
test(10, _CFG) ->
    trie:get_all(1, trie01),
    success;
test(11, CFG) ->    
    Meta = 0,
    V1 = <<2,3>>,
    Key1 = 1,
    Key2 = 2,
    Key3 = 257,
    Key4 = 513,
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    Root = trie:put(Key1, V1, Meta, Root0, trie01),
    %Root = trie:put(Key3, V1, Meta, Root01, trie01),
    {RootHash, Leaf1, Proof1} = trie:get(Key1, Root, trie01),
    {RootHash, empty, Proof2} = trie:get(Key2, Root, trie01),
    {RootHash, empty, Proof3} = trie:get(Key3, Root, trie01),
    {RootHash, empty, Proof4} = trie:get(Key4, Root, trie01),
    true = verify:proof(RootHash, Leaf1, Proof1, CFG),
    true = verify:proof(RootHash, leaf:new(Key1, V1, 0, CFG), Proof1, CFG),
    true = verify:proof(RootHash, leaf:new(Key2, empty, 0, CFG), Proof2, CFG),
    %io:fwrite({proofs, Proof2, Proof3}),
    true = verify:proof(RootHash, leaf:new(Key3, empty, 0, CFG), Proof3, CFG),
    true = verify:proof(RootHash, leaf:new(Key4, empty, 0, CFG), Proof4, CFG),
    success;
test(12, CFG) ->    
    %Times = 257,
    Times = 25,
    %Times = 5,
    ID = 2,
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    L2 = test3a(Times, Times, Root0),
    Root1 = trie:new_trie(trie01, stem:get(L2, CFG)),
    Hash = trie:root_hash(trie01, Root1),
    Hash = trie:root_hash(trie01, L2),

    %Restore data to trie.
    {Hash, unknown, _} = trie:get(ID, Root1, trie01),
    {Leaf, Root2} = restore(ID, L2, Root1),
    {Hash, Leaf, Proof} = trie:get(ID, Root2, trie01),

    %Restore our knowledge of the lack of data that ends with a leaf.
    {Hash, empty, Proof2} = trie:get(Times+1, L2, trie01),
    EmptyLeaf = leaf:new(Times, empty, 0, CFG),
    true = verify:proof(Hash, EmptyLeaf, Proof2, CFG),
    {Hash, unknown, _} = trie:get(Times+1, Root2, trie01),
    Root3 = trie:restore(EmptyLeaf, Hash, Proof2, Root2, trie01),
    {Hash, empty, _} = trie:get(Times+1, Root3, trie01),

    %Restore our knowledge of the lack of data that ends with a stem.
    Root4 = test3a(2, 2, Root0),
    Root5 = trie:new_trie(trie01, stem:get(Root4, CFG)),
    {Hash4, empty, Proof4} = trie:get(17, Root4, trie01),
    EmptyLeaf2 = leaf:new(17, empty, 0, CFG),
    true = verify:proof(Hash4, EmptyLeaf2, Proof4, CFG),
    {Hash4, unknown, _} = trie:get(17, Root5, trie01),
    Root6 = trie:restore(EmptyLeaf2, Hash4, Proof4, Root5, trie01),
    {Hash4, empty, _} = trie:get(17, Root6, trie01),
    success;

test(13, CFG) ->    

    %Restore our knowledge of various things, and then check that the information is remembered correctly.
    Times = 25,
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    L2 = test3a(Times, Times, Root0),
    Root1 = trie:new_trie(trie01, stem:get(L2, CFG)),
    [ID1, ID2, ID3] = [1, 17, 18],%adding an 18 and a 1 to this list makes it break.
    {Leaf7, Root7} = restore(ID1, L2, Root1),
    {Leaf8, Root8} = restore(ID2, L2, Root7),
    {Leaf9, Root9} = restore(ID3, L2, Root8),

    Hash = trie:root_hash(trie01, Root7),
    Hash = trie:root_hash(trie01, Root8),
    Hash = trie:root_hash(trie01, Root9),

    {Hash, Leaf7, _} = trie:get(ID1, Root7, trie01),
    {Hash, Leaf7, _} = trie:get(ID1, Root8, trie01),
    {Hash, Leaf7, _} = trie:get(ID1, Root9, trie01),
    {Hash, Leaf8, _} = trie:get(ID2, Root9, trie01),
    {Hash, Leaf9, _} = trie:get(ID3, Root9, trie01),
    success;
test(14, CFG) ->
    Loc0 = 1,
    La = <<255, 0>>,
    Lb = <<255, 1>>,
    Loc1 = trie:put(2, La, 0, Loc0, trie01),
    Loc = trie:put(1, Lb, 0, Loc1, trie01),
    Leaves = [leaf:new(1, La, 0, CFG),
	      leaf:new(2, empty, 0, CFG),
	      %leaf:new(33, La, 0, CFG),
	      leaf:new(17, La, 0, CFG)],
    %Leaves = [Leaf, Leaf3],
    %io:fwrite(packer:pack(store:batch(Leaves, Loc, CFG))),
    trie:put_batch(Leaves, Loc, trie01),
%root hash <<89,127,205,119,243,7,208,239,239,229,27,12,178,241,27,
    success;
test(15, CFG) ->
    Loc = 1,
    La = <<255, 0>>,
    Leaf = leaf:new(1, La, 0, CFG),
    %Leaf2 = leaf:new(33, La, 0, CFG),
    Leaf3 = leaf:new(17, La, 0, CFG),
    %Leaves = [Leaf, Leaf2, Leaf3],
    Leaves = [Leaf, Leaf3],
    Loc2 = trie:put(1, La, 0, Loc, trie01),
    Loc3 = trie:put(17, La, 0, Loc2, trie01),
    io:fwrite("loc 3 is "),
    io:fwrite(integer_to_list(Loc3)),
    io:fwrite("\n"),
%root hash matches test 14. {<<89,127,205,119,243,7,208,239,239,229,27,12,178,241,27,
    success;
test(16, CFG) ->
    %prune test.
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    La = <<255, 0>>,
    Lb = <<255, 1>>,
    Leaves1 = [leaf:new(1, La, 0, CFG),
	      leaf:new(2, La, 0, CFG),
	      %leaf:new(33, La, 0, CFG),
	      leaf:new(17, La, 0, CFG)],
    Leaves2 = [leaf:new(1, Lb, 0, CFG),
	      leaf:new(3, Lb, 0, CFG)],
    Leaves3 = [leaf:new(1, La, 0, CFG),
	      leaf:new(4, La, 0, CFG)],
    Old = trie:put_batch(Leaves1, Root0, trie01),
    New = trie:put_batch(Leaves2, Old, trie01),
    %insert a batch to get oldroot old,
    %insert a batch to get new
    Ls = trie:garbage(Old, New, trie01),
    io:fwrite("prune removed these "),
    io:fwrite(packer:pack(Ls)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(element(2, trie:get(1, New, trie01)))),
    Final = trie:put_batch(Leaves3, New, trie01),
    io:fwrite(packer:pack(element(2, trie:get(1, Final, trie01)))),
    %make sure we can still look up stuff from New.
    success;
test(17, CFG) ->
    %prune test.
    %Root0 = 1,
    Root0 = cfg:empty(CFG),
    La = <<255, 0>>,
    Lb = <<255, 1>>,
    Leaves1 = [leaf:new(1, La, 0, CFG),
	      leaf:new(2, La, 0, CFG),
	      %leaf:new(33, La, 0, CFG),
	      leaf:new(17, La, 0, CFG)],
    Leaves2 = [leaf:new(1, Lb, 0, CFG),
	      leaf:new(3, Lb, 0, CFG)],
    Leaves3 = [leaf:new(1, La, 0, CFG),
	      leaf:new(4, La, 0, CFG)],
    Old = trie:put_batch(Leaves1, Root0, trie01),
    New = trie:put_batch(Leaves2, Old, trie01),
    %insert a batch to get oldroot old,
    %insert a batch to get new
    Ls = trie:garbage(New, Old, trie01),
    %Ls = prune2:stem(New, Old, trie:cfg(trie01)),
    io:fwrite("garbage removed these "),
    io:fwrite(packer:pack(Ls)),
    io:fwrite("\n"),
    io:fwrite(packer:pack(element(2, trie:get(1, Old, trie01)))),
    Final = trie:put_batch(Leaves3, Old, trie01),
    io:fwrite(packer:pack(element(2, trie:get(1, Final, trie01)))),
    io:fwrite("\n"),
    io:fwrite("final pointer "),
    io:fwrite(integer_to_list(Final)),
    io:fwrite("\n"),
    %make sure we can still look up stuff from New.
    success;
test(18, CFG) ->
    %Proof2 = verify:update_proof(Leaf2, Proof, CFG),
    Loc = 1,
    Times = 1000,
    NewLoc = test3a(Times, Times, Loc),
    %test3b(Times, NewLoc, CFG),
    {Hash4, Value4, Proof4} = trie:get(4, NewLoc, ?ID),
    {Hash5, Value5, Proof5} = trie:get(5, NewLoc, ?ID),
    Leaf = leaf:new(5, <<0, 1>>, 0, CFG),
    Proof2 = verify:update_proof(Leaf, Proof5, CFG),
    NewRoot = stem:hash(hd(lists:reverse(Proof2)), CFG),
    true = verify:proof(NewRoot, Leaf, Proof2, CFG),

    [Proof3|_] = verify:update_proofs(
		[{Leaf, Proof5}|
		 [{leaf:new(4, <<0, 1>>, 0, CFG), 
		   Proof4}|[]]], CFG),
    NewRoot2 = stem:hash(hd(lists:reverse(Proof3)), CFG),
    true = verify:proof(NewRoot2, Leaf, Proof3, CFG),

    success.
    
    
    
    
restore(ID, FilledTree, NewTree) ->    
    {Hash, Leaf, Proof} = trie:get(ID, FilledTree, trie01),
    %{Hash, unknown, _} = trie:get(ID, NewTree, trie01),
    {Leaf, trie:restore(Leaf, Hash, Proof, NewTree, trie01)}.
    

test3a(0, _, L) -> L;
test3a(N, Times, Loc) -> %load up the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Meta = 0,
    NewLoc = trie:put(Times + 1 - N, <<N:16>>, Meta, Loc, ?ID),
    test3a(N-1, Times, NewLoc).
    
test3b(0, L, _CFG) -> L;
test3b(N, Loc, CFG) ->  %check that everything is in the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    {Hash, Value, Proof} = trie:get(N, Loc, ?ID),
    true = verify:proof(Hash, Value, Proof, CFG), 
    test3b(N-1, Loc, CFG).

