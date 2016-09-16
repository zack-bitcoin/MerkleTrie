-module(test_trie).
-export([test/0,test7/3]).

-define(ID, trie01).

test() ->
    WS = trie:ws(?ID),
    LS = WS + trie:m(?ID) + trie:s(?ID),
    M = trie:m(?ID),
    io:fwrite("test 1\n"),
    test1(WS, LS, M),
    io:fwrite("test 2\n"),
    test2(WS, LS, M),
    io:fwrite("test 3\n"),
    test3(WS, LS, M),
    io:fwrite("test 4\n"),
    test4(WS, LS, M),
    io:fwrite("test 5\n"),
    test5(WS, LS, M),
    io:fwrite("test 6\n"),
    test6(WS, LS, M),
    io:fwrite("test 7\n"),
    test7(WS, LS, M).
    

test1(WS, LS, M) ->
    Nib1 = 4,
    Nib2 = 2,
    L = <<Nib1:4,Nib2:4,0,0,0,0>>,
    Lflip = <<0,0,0,0,Nib1:4,Nib2:4>>,
    Lb = <<255,255>>,
    Weight = 0,
    Weight = 0,
    <<Laa:40>> = Lflip,
    <<Lbaa:16>> = Lb,
    LeafAB = leaf:new(Laa, Weight, Lbaa), 
    Lc = leaf:serialize(LeafAB, WS, LS),
    Loc1 = dump:put(Lc, ids:leaf(?ID)),
    LH = leaf:hash(LeafAB, WS, LS),
    S1 = stem:new(Nib2, 2, Loc1, 0, LH),
    Loc2 = stem:put(S1, WS, ?ID),
    SH = stem:hash(S1, WS),
    S = stem:new(Nib1, 1, Loc2, 0, SH),
    Loc3 = stem:put(S, WS, ?ID),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S, WS),
    Weight = 0,
    X = {RootHash, LeafAB, [stem:hashes(S1), stem:hashes(S)]},
    Proof = [stem:hashes(S1), stem:hashes(S)], 
    {RootHash, LeafAB, _} = get:get(L, Loc3, ?ID, WS, LS),%Path, Root
    X = get:get(L, Loc3, ?ID, WS, LS),%Path, Root
    {_, LeafAB, Proof} = X,
    Weight = leaf:weight(LeafAB),
    true = verify:proof(RootHash, LeafAB, Proof, WS, LS, M),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = <<Nib3:4,Nib4:4,0,0,0,0>>,
    L2flip = <<0,0,0,0,Nib3:4,Nib4:4>>,
    L2b = <<255,255>>,
    <<Lbb:40>> = L2flip,
    <<Lbbb:16>> = L2b,
    Leafbb = leaf:new(Lbb, Weight, Lbbb),
    L2c = leaf:serialize(Leafbb, WS, LS),
    Loc4 = dump:put(L2c, ids:leaf(?ID)),
    LH2 = leaf:hash(Leafbb, WS, LS),
    S2 = stem:new(Nib4, 2, Loc4, 0, LH2),
    Loc5 = stem:put(S2, WS, ?ID),
    SH2 = stem:hash(S2, WS),
    S3 = stem:add(S, Nib3, 1, Loc5, 0, SH2),
    Loc6 = stem:put(S3, WS, ?ID),
    RootHash2 = stem:hash(S3, WS),
    Proof2 = [stem:hashes(S2), stem:hashes(S3)],
    X2 = {RootHash2, Leafbb, Proof2},
    X2 = get:get(L2, Loc6, ?ID, WS, LS),
    true = verify:proof(RootHash2, Leafbb, Proof2, WS, LS, M),
    Nib5 = 4,
    Nib6 = 2,
    L3 = <<Nib5:4,Nib6:4,0:4,1:4,0,0,0>>,
    L3flip = <<0,0,0,0:4,1:4,Nib5:4,Nib6:4>>,
    L3b = <<255,255>>,
    <<L3abc:40>> = L3flip, 
    <<L3value:16>> = L3b,
    Leafcc = leaf:new(L3abc, Weight, L3value), 
    {_, Loc7, _} = store:store(Leafcc, Loc6, ?ID, WS, LS, M),
    trie:garbage([Loc7], ?ID),%problem
    ReplaceStem = <<0:(8*(dump:word(ids:stem(?ID))))>>,
    0 = dump:put(ReplaceStem, ids:stem(?ID)),
    {PP4,Leafcc,PP5} = get:get(L3, Loc7, ?ID, WS, LS),
    true = verify:proof(PP4,Leafcc,PP5,WS,LS,M),
    ok.

test2(WS, LS, M) ->
    Loc = 0,
    L = <<0:4,0:4,0:4,0:4,0,0,0>>,
    <<La:16>> = <<255, 0>>,
    Weight = 0,
    Leaf = leaf:new(0, Weight, La),
    store:store(Leaf, Loc, ?ID, WS, LS, M).

test3(WS, LS, M) -> 
    Loc = 0,
    Times = 10000,
    {Keys, NewLoc} = test3a(Times, [], Loc, WS),
    test3b(1, Keys, NewLoc, WS, LS, M).
test3a(0, Keys, L, _) -> {Keys, L};
test3a(N, Keys, Loc, WS) -> %load up the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Weight = 0,
    {Key, NewLoc} = trie:put(N, Loc, Weight, ?ID),
    test3a(N-1, [Key|Keys], NewLoc, WS).
test3b(_, [], L, _, _, _) -> L;
test3b(N, [Key|T], Loc, WS, LS, M) ->  %check that everything is in the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Weight = 0,
    {Hash, N, Weight, Proof} = trie:get(Key, Loc, ?ID),
    Leaf = leaf:new(Key, Weight, N),
    true = verify:proof(Hash, Leaf, Proof, WS, LS, M), 
    test3b(N+1, T, Loc, WS, LS, M).

test4(_WS, LS, M) ->
    Size = dump:word(ids:leaf(?ID)),
    Data0 = <<11:(8*Size)>>,
    Data1 = <<2:(8*Size)>>,
    Data2 = <<3:(8*Size)>>,
    Data3 = <<4:(8*Size)>>,
    A0 = dump:put(Data0, ids:leaf(?ID)),
    Data0 = dump:get(A0, ids:leaf(?ID)),
    A1 = dump:put(Data1, ids:leaf(?ID)),
    Data1 = dump:get(A1, ids:leaf(?ID)),
    dump:delete(A0, ids:leaf(?ID)),
    A0 = dump:put(Data1, ids:leaf(?ID)),
    Data1 = dump:get(A0, ids:leaf(?ID)),
    A2 = dump:put(Data2, ids:leaf(?ID)),
    Data1 = dump:get(A0, ids:leaf(?ID)),
    A3 = dump:put(Data3, ids:leaf(?ID)),
    Data1 = dump:get(A0, ids:leaf(?ID)),
    Data1 = dump:get(A1, ids:leaf(?ID)),
    Data2 = dump:get(A2, ids:leaf(?ID)),
    Data3 = dump:get(A3, ids:leaf(?ID)),

    Size2 = dump:word(ids:stem(?ID)),
    Data02 = <<11:(8*Size2)>>,
    Data12 = <<2:(8*Size2)>>,
    Data22 = <<3:(8*Size2)>>,
    Data32 = <<4:(8*Size2)>>,
    A02 = dump:put(Data02, ids:stem(?ID)),
    A12 = dump:put(Data12, ids:stem(?ID)),
    A22 = dump:put(Data22, ids:stem(?ID)),
    A32 = dump:put(Data32, ids:stem(?ID)),
    Data02 = dump:get(A02, ids:stem(?ID)),
    Data12 = dump:get(A12, ids:stem(?ID)),
    Data22 = dump:get(A22, ids:stem(?ID)),
    Data32 = dump:get(A32, ids:stem(?ID)),
    dump:delete(A02, ids:stem(?ID)),
    success.

test5(WS, LS, M) ->
    Root0 = 0,
    <<V1:16>> = <<1,1>>,
    <<V2:16>> = <<1,2>>,
    <<V3:16>> = <<1,3>>,
    <<L1:40>> = <<0,0,0,0,0>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    <<L3:40>> = <<0,1,0,0,0>>,
    Weight = 0,
    Leaf1 = leaf:new(L1, Weight, V1),
    Leaf2 = leaf:new(L2, Weight, V2),
    Leaf3 = leaf:new(L3, Weight, V3),
    {_, Root1, _} = store:store(Leaf1, Root0, ?ID, WS, LS, M),
    {Hash, Root2, _} = store:store(Leaf2, Root1, ?ID, WS, LS, M),
    {Hash, Root3, _} = store:store(Leaf2, Root2, ?ID, WS, LS, M),
    {_, Root4, _} = store:store(Leaf3, Root3, ?ID, WS, LS, M),
    Lpath1 = leaf:path(Leaf1, M),
    X = [{Lpath1, Root4}],
    io:fwrite("garbage leaves\n"),
    garbage:garbage_leaves(X, trie:m(?ID), ?ID, WS, LS),%After we do garbage leaves we can't insert things into the merkle tree normally. 
    %many stems are missing, so we can't make proofs of anything we don't save, but we can still verify them.
    %We need a merkle proof of it's previous state in order to update.
    {Hash3, Leaf1, Proof} = get:get(Lpath1, Root4, ?ID, WS, LS),
    verify:proof(Hash3, Leaf1, Proof, WS, LS, M),
    ok.

test6(WS, LS, M) ->
    %The purpose of this test is to test merge.
    % The full merkel trie will be too big, most people wont keep track of it all. 
    % sometimes parts of the trie get updated that we aren't keeping track of. We need to look at the proof of their update, and update our state root accordingly.
    % We don't get a proof of the final state. We only get a proof of the initial state, and the final state. It is possible to calculate the new proof from this. The reason we don't get the new proof is because depending on which txs get accepted into the block, the root hash of the new state will be different
    Root0 = 0,
    <<V1:16>> = <<1,1>>,
    <<V2:16>> = <<1,2>>,
    <<V3:16>> = <<1,3>>,
    <<L1:40>> = <<0,0,0,0,0>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    Weight = 0,
    Leafa = leaf:new(L1, Weight, V1),
    {_, Root1, _} = store:store(Leafa, Root0, ?ID, WS, LS, M),
    {Hash0bb, Leafa, Proofa} = get:get(leaf:path(Leafa, M), Root1, ?ID, WS, LS),
    true = verify:proof(Hash0bb, Leafa, Proofa, WS, LS, M),
    Leafb = leaf:new(L2, Weight, V2),
    {_, Root2, _} = store:store(Leafb, Root1, ?ID, WS, LS, M),
    {Hash0, Leafb, Proofb} = get:get(leaf:path(Leafb, M), Root2, ?ID, WS, LS),
    true = verify:proof(Hash0, Leafb, Proofb, WS, LS, M),
    Leafc = leaf:new(L2, Weight, V3),
    {Hash, Root3, _} = store:store(Leafc, Root2, ?ID, WS, LS, M),
    {Hasha, _, _} = store:store(Leafc, Root1, ?ID, WS, LS, M),
    Hasha = Hash,
    {Hash, Leafc, Proofc} = get:get(leaf:path(Leafc, M), Root3, ?ID, WS, LS),
    true = verify:proof(Hash, Leafc, Proofc, WS, LS, M),
    GL = [{leaf:path(Leafa, M), Root1}],
    {Hash, _, _} = store:store(Leafc, Root1, ?ID, WS, LS, M),
    {_, Leafa, _} = get:get(leaf:path(Leafa, M), Root1, ?ID, WS, LS),
    garbage:garbage_leaves(GL, trie:m(?ID), ?ID, WS, LS),
    {_, Leafa, _} = get:get(leaf:path(Leafa, M), Root1, ?ID, WS, LS),
    %it is over-writing the old leaf.
    {Hash, _, _} = store:store(Leafc, Root1, ?ID, WS, LS, M),
    Root4 = merge:doit([Leafc], Hash, Root1, WS, LS, M, ?ID),
    {Hash, Leafa, B2} = get:get(leaf:path(Leafa, M), Root4, ?ID, WS, LS),
    true = verify:proof(Hash, Leafa, B2, WS, LS, M).
% the current implementation is very innefficient. It stores the entire proof onto the hard drive

test7(WS, LS, M) ->
    Root0 = 0,
    <<V1:16>> = <<1,1>>,
    <<V2:16>> = <<1,2>>,
    <<L1:40>> = <<0,0,0,0,0>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    Weight = 1,
    Leaf1 = leaf:new(L1, Weight, V1),
    Leaf2 = leaf:new(L2, Weight, V2),
    {_, Root1, _} = store:store(Leaf1, Root0, ?ID, WS, LS, M),
    {Hash0bb, Leaf1, B0bb} = get:get(leaf:path(Leaf1, M), Root1, ?ID, WS, LS),
    true = verify:proof(Hash0bb, Leaf1, B0bb, WS, LS, M),
    {_, Root2, _} = store:store(Leaf2, Root1, ?ID, WS, LS, M),
    {Hash0, Leaf2, B0} = get:get(leaf:path(Leaf2, M), Root2, ?ID, WS, LS),
    true = verify:proof(Hash0, Leaf2, B0, WS, LS, M),
    io:fwrite("here\n"),
    Stem = stem:get(Root2, WS, ?ID),
    FS = {2,0,0,0,
	  0,0,0,0,
	  0,0,0,0,
	  0,0,0,0},
    FS = stem:weights(Stem).
    
    
