-module(test_trie).
-export([test/0,test7/1,test3a/3]).

-define(ID, trie01).

test() ->
    CFG = trie:cfg(?ID),
    io:fwrite("test 1\n"),
    test1(CFG),
    io:fwrite("test 2\n"),
    test2(CFG),
    io:fwrite("test 3\n"),
    test3(CFG),
    io:fwrite("test 4\n"),
    test4(CFG),
    io:fwrite("test 5\n"),
    test5(CFG),
    io:fwrite("test 6\n"),
    test6(CFG),
    io:fwrite("test 7\n"),
    test7(CFG).
    

test1(CFG) ->
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
    Lc = leaf:serialize(LeafAB, CFG),
    Loc1 = dump:put(Lc, ids:leaf(CFG)),
    LH = leaf:hash(LeafAB, CFG),
    S1 = stem:new(Nib2, 2, Loc1, 0, LH),
    Loc2 = stem:put(S1, CFG),
    SH = stem:hash(S1, CFG),
    S = stem:new(Nib1, 1, Loc2, 0, SH),
    Loc3 = stem:put(S, CFG),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S, CFG),
    Weight = 0,
    X = {RootHash, LeafAB, [stem:hashes(S1), stem:hashes(S)]},
    Proof = [stem:hashes(S1), stem:hashes(S)], 
    {RootHash, LeafAB, _} = get:get(L, Loc3, CFG),%Path, Root
    X = get:get(L, Loc3, CFG),%Path, Root
    {_, LeafAB, Proof} = X,
    Weight = leaf:weight(LeafAB),
    true = verify:proof(RootHash, LeafAB, Proof, CFG),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = <<Nib3:4,Nib4:4,0,0,0,0>>,
    L2flip = <<0,0,0,0,Nib3:4,Nib4:4>>,
    L2b = <<255,255>>,
    <<Lbb:40>> = L2flip,
    <<Lbbb:16>> = L2b,
    Leafbb = leaf:new(Lbb, Weight, Lbbb),
    L2c = leaf:serialize(Leafbb, CFG),
    Loc4 = dump:put(L2c, ids:leaf(CFG)),
    LH2 = leaf:hash(Leafbb, CFG),
    S2 = stem:new(Nib4, 2, Loc4, 0, LH2),
    Loc5 = stem:put(S2, CFG),
    SH2 = stem:hash(S2, CFG),
    S3 = stem:add(S, Nib3, 1, Loc5, 0, SH2),
    Loc6 = stem:put(S3, CFG),
    RootHash2 = stem:hash(S3, CFG),
    Proof2 = [stem:hashes(S2), stem:hashes(S3)],
    X2 = {RootHash2, Leafbb, Proof2},
    X2 = get:get(L2, Loc6, CFG),
    true = verify:proof(RootHash2, Leafbb, Proof2, CFG),
    Nib5 = 4,
    Nib6 = 2,
    L3 = <<Nib5:4,Nib6:4,0:4,1:4,0,0,0>>,
    L3flip = <<0,0,0,0:4,1:4,Nib5:4,Nib6:4>>,
    L3b = <<255,255>>,
    <<L3abc:40>> = L3flip, 
    <<L3value:16>> = L3b,
    Leafcc = leaf:new(L3abc, Weight, L3value), 
    {_, Loc7, _} = store:store(Leafcc, Loc6, CFG),
    trie:garbage([Loc7], ?ID),
    trie:cfg(?ID),
    ReplaceStem = <<0:(8*(dump:word(ids:stem(CFG))))>>,
    0 = dump:put(ReplaceStem, ids:stem(CFG)),
    {PP4,Leafcc,PP5} = get:get(L3, Loc7, CFG),
    true = verify:proof(PP4,Leafcc,PP5,CFG),
    ok.

test2(CFG) ->
    Loc = 0,
    L = <<0:4,0:4,0:4,0:4,0,0,0>>,
    <<La:16>> = <<255, 0>>,
    Weight = 0,
    Leaf = leaf:new(0, Weight, La),
    store:store(Leaf, Loc, CFG).

test3(CFG) -> 
    Loc = 0,
    Times = 10000,
    {Keys, NewLoc} = test3a(Times, [], Loc),
    test3b(1, Keys, NewLoc, CFG).
test3a(0, Keys, L) -> {Keys, L};
test3a(N, Keys, Loc) -> %load up the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    {Key, NewLoc} = trie:put(N, Loc, 0, ?ID),
    test3a(N-1, [Key|Keys], NewLoc).
test3b(_, [], L, CFG) -> L;
test3b(N, [Key|T], Loc, CFG) ->  %check that everything is in the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Weight = 0,
    {Hash, Leaf, Proof} = trie:get(Key, Loc, ?ID),
    true = verify:proof(Hash, Leaf, Proof, CFG), 
    test3b(N+1, T, Loc, CFG).

test4(CFG) ->
    Size = dump:word(ids:leaf(CFG)),
    Size = cfg:leaf(CFG),
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
    success.

test5(CFG) ->
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
    {_, Root1, _} = store:store(Leaf1, Root0, CFG),
    {Hash, Root2, _} = store:store(Leaf2, Root1, CFG),
    {Hash, Root3, _} = store:store(Leaf2, Root2, CFG),
    {_, Root4, _} = store:store(Leaf3, Root3, CFG),
    Lpath1 = leaf:path(Leaf1, CFG),
    X = [{Lpath1, Root4}],
    io:fwrite("garbage leaves\n"),
    garbage:garbage_leaves(X, CFG),%After we do garbage leaves we can't insert things into the merkle tree normally. 
    %many stems are missing, so we can't make proofs of anything we don't save, but we can still verify them.
    %We need a merkle proof of it's previous state in order to update.
    timer:sleep(7000),
    {Hash3, Leaf1, Proof} = get:get(Lpath1, Root4, CFG),
    verify:proof(Hash3, Leaf1, Proof, CFG),
    ok.

test6(CFG) ->
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
    {_, Root1, _} = store:store(Leafa, Root0, CFG),
    {Hash0bb, Leafa, Proofa} = get:get(leaf:path(Leafa, CFG), Root1, CFG),
    true = verify:proof(Hash0bb, Leafa, Proofa, CFG),
    Leafb = leaf:new(L2, Weight, V2),
    {_, Root2, _} = store:store(Leafb, Root1, CFG),
    {Hash0, Leafb, Proofb} = get:get(leaf:path(Leafb, CFG), Root2, CFG),
    true = verify:proof(Hash0, Leafb, Proofb, CFG),
    Leafc = leaf:new(L2, Weight, V3),
    {Hash, Root3, _} = store:store(Leafc, Root2, CFG),
    {Hasha, _, _} = store:store(Leafc, Root1, CFG),
    Hasha = Hash,
    {Hash, Leafc, Proofc} = get:get(leaf:path(Leafc, CFG), Root3, CFG),
    true = verify:proof(Hash, Leafc, Proofc, CFG),
    GL = [{leaf:path(Leafa, CFG), Root1}],
    {Hash, _, _} = store:store(Leafc, Root1, CFG),
    {_, Leafa, _} = get:get(leaf:path(Leafa, CFG), Root1, CFG),
    garbage:garbage_leaves(GL, CFG),
    timer:sleep(7000),
    {_, Leafa, _} = get:get(leaf:path(Leafa, CFG), Root1, CFG),
    %it is over-writing the old leaf.
    {Hash, _, _} = store:store(Leafc, Root1, CFG),
    Root4 = merge:doit([Leafc], Hash, Root1, CFG),
    {Hash, Leafa, B2} = get:get(leaf:path(Leafa, CFG), Root4, CFG),
    true = verify:proof(Hash, Leafa, B2, CFG).
% the current implementation is very innefficient. It stores the entire proof onto the hard drive

test7(CFG) ->
    Root0 = 0,
    <<V1:16>> = <<1,1>>,
    <<V2:16>> = <<1,2>>,
    <<L1:40>> = <<0,0,0,0,0>>,
    <<L2:40>> = <<0,16,0,0,0>>,
    Weight = 1,
    Leaf1 = leaf:new(L1, Weight, V1),
    Leaf2 = leaf:new(L2, Weight, V2),
    {_, Root1, _} = store:store(Leaf1, Root0, CFG),
    {Hash0bb, Leaf1, B0bb} = get:get(leaf:path(Leaf1, CFG), Root1, CFG),
    true = verify:proof(Hash0bb, Leaf1, B0bb, CFG),
    {_, Root2, _} = store:store(Leaf2, Root1, CFG),
    {Hash0, Leaf2, B0} = get:get(leaf:path(Leaf2, CFG), Root2, CFG),
    true = verify:proof(Hash0, Leaf2, B0, CFG),
    io:fwrite("here\n"),
    Stem = stem:get(Root2, CFG),
    FS = {2,0,0,0,
	  0,0,0,0,
	  0,0,0,0,
	  0,0,0,0},
    FS = stem:weights(Stem).
    
    
