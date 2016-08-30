-module(test_trie).
-export([test/0]).

test() ->
    io:fwrite("test 1\n"),
    test1(),
    io:fwrite("test 2\n"),
    test2(),
    io:fwrite("test 3\n"),
    test3(),
    io:fwrite("test 4\n"),
    test4(),
    io:fwrite("test 5\n"),
    test5(),
    io:fwrite("test 6\n"),
    test6().
    

test1() ->
    Nib1 = 4,
    Nib2 = 2,
    L = <<Nib1:4,Nib2:4,0,0,0,
	  0,0,0,0,
	  0,0,0,0>>,
    Lb = <<255,255>>,
    Lc = <<L/binary, Lb/binary>>,
    Loc1 = dump:put(Lc, leaf),
    LH = hash:doit(Lc),
    S1 = stem:new(Nib2, 2, Loc1, LH),
    Loc2 = dump:put(stem:serialize(S1), stem),
    SH = stem:hash(S1),
    S = stem:new(Nib1, 1, Loc2, SH),
    Loc3 = dump:put(stem:serialize(S), stem),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S),
    X = {RootHash, L, Lb, [stem:hashes(S1), stem:hashes(S)]},
    X = get:get(L, Loc3),%Path, Root
    {_, _, _, Proof} = X,
    true = verify:proof(RootHash, Lc, Proof),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = <<Nib3:4,Nib4:4,0,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    L2b = <<255,255>>,
    L2c = <<L2/binary, L2b/binary>>,
    Loc4 = dump:put(L2c, leaf),
    LH2 = hash:doit(L2c),
    S2 = stem:new(Nib4, 2, Loc4, LH2),
    Loc5 = dump:put(stem:serialize(S2), stem),
    SH2 = stem:hash(S2),
    %S3a = stem:new(Nib2, 1, Loc3, SH),
    S3 = stem:add(S, Nib3, 1, Loc5, SH2),
    Loc6 = dump:put(stem:serialize(S3), stem),
    RootHash2 = stem:hash(S3),
    X2 = {RootHash2, L2, L2b, [stem:hashes(S2), stem:hashes(S3)]},
    X2 = get:get(L2, Loc6),
    {_, _, _, Proof2} = X2,
    true = verify:proof(RootHash2, L2c, Proof2),
    {PP1, L2, L2b, PP3} = get:get(L2, Loc6),
    true = verify:proof(PP1, L2c, PP3),

    Nib5 = 4,
    Nib6 = 2,
    L3 = <<Nib5:4,Nib6:4,0:4,1:4,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    L3b = <<255,255>>,
    L3c = <<L3/binary, L3b/binary>>,
    {_, Loc7, _} = store:store(L3, L3b, Loc6),
    trie:garbage([Loc7]),
    ReplaceStem = <<0:(8*(dump:word(stem)))>>,
    0 = dump:put(ReplaceStem, stem),
    {PP4,L3,L3b,PP5} = get:get(L3, Loc7),
    true = verify:proof(PP4,L3c,PP5),
    ok.

test2() ->
    Loc = 0,
    L = <<0:4,0:4,0:4,0:4, 0,0,0,0,0,0,
	   0,0,0,0>>,
    La = <<255, 0>>,
    store:store(L, La, Loc).

test3() -> 
    Loc = 0,
    Times = 1000,
    NewLoc = test3a(0, Times, Loc),
    test3b(0, Times, NewLoc).
test3a(N, N, L) -> L;
test3a(N, M, Loc) -> %load up the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Key = <<N:16>>,
    Value = Key,
    NewLoc = trie:put(Key, Value, Loc),
    test3a(N+1, M, NewLoc).
test3b(N, N, L) -> L;
test3b(N, M, Loc) ->  %check that everything is in the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    Key = <<N:16>>,
    Value = Key,
    {Hash, Value, Proof} = trie:get(Key, Loc),
    verify:proof(Hash, Key, Value, Proof),
    test3b(N+1, M, Loc).

test4() ->
    Size = dump:word(leaf),
    Data0 = <<11:(8*Size)>>,
    Data1 = <<2:(8*Size)>>,
    Data2 = <<3:(8*Size)>>,
    Data3 = <<4:(8*Size)>>,
    A0 = dump:put(Data0, leaf),
    Data0 = dump:get(A0, leaf),
    A1 = dump:put(Data1, leaf),
    Data1 = dump:get(A1, leaf),
    dump:delete(A0, leaf),
    A0 = dump:put(Data1, leaf),
    Data1 = dump:get(A0, leaf),
    A2 = dump:put(Data2, leaf),
    Data1 = dump:get(A0, leaf),
    A3 = dump:put(Data3, leaf),
    Data1 = dump:get(A0, leaf),
    Data1 = dump:get(A1, leaf),
    Data2 = dump:get(A2, leaf),
    Data3 = dump:get(A3, leaf),

    Size2 = dump:word(stem),
    Data02 = <<11:(8*Size2)>>,
    Data12 = <<2:(8*Size2)>>,
    Data22 = <<3:(8*Size2)>>,
    Data32 = <<4:(8*Size2)>>,
    A02 = dump:put(Data02, stem),
    A12 = dump:put(Data12, stem),
    A22 = dump:put(Data22, stem),
    A32 = dump:put(Data32, stem),
    Data02 = dump:get(A02, stem),
    Data12 = dump:get(A12, stem),
    Data22 = dump:get(A22, stem),
    Data32 = dump:get(A32, stem),
    dump:delete(A02, stem),
    success.

test5() ->
    Root0 = 0,
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    L1 = <<0,0,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    L2 = <<0,16,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    L3 = <<0,1,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    {_, Root1, _} = store:store(L1, V1, Root0),
    {Hash, Root2, _} = store:store(L2, V2, Root1),
    {Hash, Root3, _} = store:store(L2, V2, Root2),
    {_, Root4, _} = store:store(L3, V3, Root3),
    X = [{L1, Root4}],
    io:fwrite("garbage leaves\n"),
    garbage:garbage_leaves(X),%After we do garbage leaves we can't insert things into the merkle tree normally. 
    %many stems are missing, so we can't make proofs of anything we don't save, but we can still verify them.
    %We need a merkle proof of it's previous state in order to update.
    {Hash3, L1, V1, Proof} = get:get(L1, Root4),
    verify:proof(Hash3, L1, V1, Proof),
    ok.

test6() ->
    %The purpose of this test is to test merge.
    % The full merkel trie will be too big, most people wont keep track of it all. 
    % sometimes parts of the trie get updated that we aren't keeping track of. We need to look at the proof of their update, and update our state root accordingly.
    % We don't get a proof of the final state. We only get a proof of the initial state, and the final state. It is possible to calculate the new proof from this. The reason we don't get the new proof is because depending on which txs get accepted into the block, the root hash of the new state will be different
    Root0 = 0,
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    L1 = <<0,0,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    L2 = <<0,16,0,0,
	   0,0,0,0,
	   0,0,0,0>>,
    {_, Root1, _} = store:store(L1, V1, Root0),
    {Hash0bb, L1, V1, B0bb} = get:get(L1, Root1),
    true = verify:proof(Hash0bb, <<L1/bitstring, V1/bitstring>>, B0bb),
    {_, Root2, _} = store:store(L2, V2, Root1),
    {Hash0, L2, V2, B0} = get:get(L2, Root2),
    true = verify:proof(Hash0, <<L2/bitstring, V2/bitstring>>, B0),
    {Hash0b, L1, V1, B0b} = get:get(L1, Root2),
    true = verify:proof(Hash0b, <<L1/bitstring, V1/bitstring>>, B0b),
    {Hash, Root3, _} = store:store(L2, V3, Root2),
    {Hasha, Roota, _} = store:store(L2, V3, Root1),
    %the problem is that in the second case, the stem make it look like both leaves hold the data for leaf 2, but the leaves hold the right data. The first case looks right. it stores 1,1 and 1,3.
    Hasha = Hash,
    {Hash, L2, V3, B} = get:get(L2, Root3),
    true = verify:proof(Hash, <<L2/bitstring, V3/bitstring>>, B),
    GL = [{L1, Root1}],
    {Hash, _, _} = store:store(L2, V3, Root1),
    {_, _, V1, _} = get:get(L1, Root1),
    garbage:garbage_leaves(GL),
    {_, _, V1, _} = get:get(L1, Root1),
    %it is over-writing the old leaf.
    {Hash, _, _} = store:store(L2, V3, Root1),
    Root4 = merge:doit([{L2, V3, B0}], Hash, Root1),
    {Hash, L1, V1, B2} = get:get(L1, Root4),
    true = verify:proof(Hash, <<L1/bitstring, V1/bitstring>>, B2).
% the current implementation is very innefficient. It stores the entire proof into memory.
