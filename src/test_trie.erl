-module(test_trie).
-export([test/0]).

-define(ID, trie01).

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
    L = <<Nib1:4,Nib2:4,0,0,0,0>>,
    Lb = <<255,255>>,
    Lc = <<L/binary, Lb/binary>>,
    Loc1 = dump:put(Lc, ids:leaf(?ID)),
    LH = hash:doit(Lc),
    S1 = stem:new(Nib2, 2, Loc1, LH),
    Loc2 = dump:put(stem:serialize(S1), ids:stem(?ID)),
    SH = stem:hash(S1),
    S = stem:new(Nib1, 1, Loc2, SH),
    Loc3 = dump:put(stem:serialize(S), ids:stem(?ID)),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S),
    X = {RootHash, L, Lb, [stem:hashes(S1), stem:hashes(S)]},
    X = get:get(L, Loc3, ?ID),%Path, Root
    {_, _, _, Proof} = X,
    true = verify:proof(RootHash, Lc, Proof),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = <<Nib3:4,Nib4:4,0,0,0,0>>,
    L2b = <<255,255>>,
    L2c = <<L2/binary, L2b/binary>>,
    Loc4 = dump:put(L2c, ids:leaf(?ID)),
    LH2 = hash:doit(L2c),
    S2 = stem:new(Nib4, 2, Loc4, LH2),
    Loc5 = dump:put(stem:serialize(S2), ids:stem(?ID)),
    SH2 = stem:hash(S2),
    %S3a = stem:new(Nib2, 1, Loc3, SH),
    S3 = stem:add(S, Nib3, 1, Loc5, SH2),
    Loc6 = dump:put(stem:serialize(S3), ids:stem(?ID)),
    RootHash2 = stem:hash(S3),
    X2 = {RootHash2, L2, L2b, [stem:hashes(S2), stem:hashes(S3)]},
    X2 = get:get(L2, Loc6, ?ID),
    {_, _, _, Proof2} = X2,
    true = verify:proof(RootHash2, L2c, Proof2),
    {PP1, L2, L2b, PP3} = get:get(L2, Loc6, ?ID),
    true = verify:proof(PP1, L2c, PP3),

    Nib5 = 4,
    Nib6 = 2,
    L3 = <<Nib5:4,Nib6:4,0:4,1:4,0,0,0>>,
    L3b = <<255,255>>,
    L3c = <<L3/binary, L3b/binary>>,
    {_, Loc7, _} = store:store(L3, L3b, Loc6, ?ID),
    trie:garbage([Loc7], ?ID),%problem
    ReplaceStem = <<0:(8*(dump:word(ids:stem(?ID))))>>,
    0 = dump:put(ReplaceStem, ids:stem(?ID)),
    {PP4,L3,L3b,PP5} = get:get(L3, Loc7, ?ID),
    true = verify:proof(PP4,L3c,PP5),
    ok.

test2() ->
    Loc = 0,
    L = <<0:4,0:4,0:4,0:4,0,0,0>>,
    La = <<255, 0>>,
    store:store(L, La, Loc, ?ID).

test3() -> 
    Loc = 0,
    Times = 1000,
    {Keys, NewLoc} = test3a(Times, [], Loc),
    test3b(1, Keys, NewLoc).
test3a(0, Keys, L) -> {Keys, L};
test3a(N, Keys, Loc) -> %load up the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    %Key = <<N:16>>,
    %Value = Key,
    Value = <<N:16>>,
    {Key, NewLoc} = trie:put(Value, Loc, ?ID),
    test3a(N-1, [Key|Keys], NewLoc).
test3b(_, [], L) -> L;
test3b(N, [Key|T], Loc) ->  %check that everything is in the trie
    if
	(N rem 100) == 0 ->
	    io:fwrite(integer_to_list(N)),
	    io:fwrite("\n");
	true -> ok
    end,
    %Key = <<N:16>>,
    %Value = Key,
    Value = <<N:16>>,
    {Hash, Value, Proof} = trie:get(Key, Loc, ?ID),
    true = verify:proof(Hash, trie:integer2path(Key, trie:m(?ID)), Value, Proof),
    test3b(N+1, T, Loc).

test4() ->
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

test5() ->
    Root0 = 0,
    V1 = <<1,1>>,
    V2 = <<1,2>>,
    V3 = <<1,3>>,
    L1 = <<0,0,0,0,0>>,
    L2 = <<0,16,0,0,0>>,
    L3 = <<0,1,0,0,0>>,
    {_, Root1, _} = store:store(L1, V1, Root0, ?ID),
    {Hash, Root2, _} = store:store(L2, V2, Root1, ?ID),
    {Hash, Root3, _} = store:store(L2, V2, Root2, ?ID),
    {_, Root4, _} = store:store(L3, V3, Root3, ?ID),
    X = [{L1, Root4}],
    io:fwrite("garbage leaves\n"),
    garbage:garbage_leaves(X, trie:m(?ID), ?ID),%After we do garbage leaves we can't insert things into the merkle tree normally. 
    %many stems are missing, so we can't make proofs of anything we don't save, but we can still verify them.
    %We need a merkle proof of it's previous state in order to update.
    {Hash3, L1, V1, Proof} = get:get(L1, Root4, ?ID),
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
    L1 = <<0,0,0,0,0>>,
    L2 = <<0,16,0,0,0>>,
    {_, Root1, _} = store:store(L1, V1, Root0, ?ID),
    {Hash0bb, L1, V1, B0bb} = get:get(L1, Root1, ?ID),
    true = verify:proof(Hash0bb, <<L1/bitstring, V1/bitstring>>, B0bb),
    {_, Root2, _} = store:store(L2, V2, Root1, ?ID),
    {Hash0, L2, V2, B0} = get:get(L2, Root2, ?ID),
    true = verify:proof(Hash0, <<L2/bitstring, V2/bitstring>>, B0),
    {Hash0b, L1, V1, B0b} = get:get(L1, Root2, ?ID),
    true = verify:proof(Hash0b, <<L1/bitstring, V1/bitstring>>, B0b),
    {Hash, Root3, _} = store:store(L2, V3, Root2, ?ID),
    {Hasha, _, _} = store:store(L2, V3, Root1, ?ID),
    %the problem is that in the second case, the stem make it look like both leaves hold the data for leaf 2, but the leaves hold the right data. The first case looks right. it stores 1,1 and 1,3.
    Hasha = Hash,
    {Hash, L2, V3, B} = get:get(L2, Root3, ?ID),
    true = verify:proof(Hash, <<L2/bitstring, V3/bitstring>>, B),
    GL = [{L1, Root1}],
    {Hash, _, _} = store:store(L2, V3, Root1, ?ID),
    {_, _, V1, _} = get:get(L1, Root1, ?ID),
    garbage:garbage_leaves(GL, trie:m(?ID), ?ID),
    {_, _, V1, _} = get:get(L1, Root1, ?ID),
    %it is over-writing the old leaf.
    {Hash, _, _} = store:store(L2, V3, Root1, ?ID),
    Root4 = merge:doit([{L2, V3, B0}], Hash, Root1, ?ID),
    {Hash, L1, V1, B2} = get:get(L1, Root4, ?ID),
    true = verify:proof(Hash, <<L1/bitstring, V1/bitstring>>, B2).
% the current implementation is very innefficient. It stores the entire proof onto the hard drive
