-module(test).
-export([test/0,test2/0,test3/0]).

test() ->
    Nib1 = 4,
    Nib2 = 2,
    L = <<Nib1:4,Nib2:4,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,0>>,
    Lb = <<255,255>>,
    Lc = <<L/binary, Lb/binary>>,
    Loc1 = low:write_leaf(Lc),
    LH = hash:doit(Lc),
    S1 = stem:new(Nib2, 2, Loc1, LH),
    Loc2 = low:write_stem(S1),
    SH = stem:hash(S1),
    S = stem:new(Nib1, 1, Loc2, SH),
    Loc3 = low:write_stem(S),
    %Starts with a 2-level tree with a single leaf at the end.
    RootHash = stem:hash(S),
    X = {RootHash, L, Lb, [stem:hashes(S1), stem:hashes(S)]},
    X = get:get(L, Loc3),%Path, Root
    {_, _, _, Proof} = X,
    true = verify:proof(RootHash, Lc, Proof),
    %Now we add a second element.
    Nib3 = 5,
    Nib4 = 10,
    L2 = <<Nib3:4,Nib4:4,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0>>, 
    L2b = <<255,255>>,
    L2c = <<L2/binary, L2b/binary>>,
    Loc4 = low:write_leaf(L2c),
    LH2 = hash:doit(L2c),
    S2 = stem:new(Nib4, 2, Loc4, LH2),
    Loc5 = low:write_stem(S2),
    SH2 = stem:hash(S2),
    %S3a = stem:new(Nib2, 1, Loc3, SH),
    S3 = stem:add(S, Nib3, 1, Loc5, SH2),
    Loc6 = low:write_stem(S3),
    RootHash2 = stem:hash(S3),
    X2 = {RootHash2, L2, L2b, [stem:hashes(S2), stem:hashes(S3)]},
    X2 = get:get(L2, Loc6),
    {_, _, _, Proof2} = X2,
    true = verify:proof(RootHash2, L2c, Proof2),
    {PP1, L2, L2b, PP3} = get:get(L2, Loc6),
    true = verify:proof(PP1, L2c, PP3),

    Nib5 = 4,
    Nib6 = 2,
    L3 = <<Nib5:4,Nib6:4,0:4,1:4,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0>>, 
    L3b = <<255,255>>,
    L3c = <<L3/binary, L3b/binary>>,
    {_, Loc7, _} = store:store(L3, L3b, Loc6),
    trie:garbage([Loc7]),
    ReplaceStem = <<0:(8*(low:word(stem_db)))>>,
    0 = low:write_stem(ReplaceStem),
    {PP4,L3,L3b,PP5} = get:get(L3, Loc7),
    true = verify:proof(PP4,L3c,PP5),
    ok.

test2() ->
    Loc = 0,
    L = <<0:4,0:4,0:4,0:4, 0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0>>, 
    La = <<255, 0>>,
    store:store(L, La, Loc).

test3() -> 
    Loc = 0,
    Times = 1000,
    NewLoc = test3a(0, Times, Loc),
    test3b(0, Times, NewLoc).
test3a(N, N, L) -> L;
test3a(N, M, Loc) -> %load up the trie
    Key = <<N:16>>,
    Value = Key,
    NewLoc = trie:put(Key, Value, Loc),
    test3a(N+1, M, NewLoc).
test3b(N, N, L) -> L;
test3b(N, M, Loc) ->  %check that everything is in the trie
    Key = <<N:16>>,
    Value = Key,
    {Hash, Value, Proof} = trie:get(Key, Loc),
    verify:proof(Hash, Key, Value, Proof),
    test3b(N+1, M, Loc).

%Loc = put(Key, Value, 0),
%{Hash, Value, Proof} = get(Key, Loc),
%verify_proof(Hash, Key, Value, Proof).
    
test4() ->
    %test low
    Size = low:word(leaf_db),
    Data0 = <<11:(8*Size)>>,
    Data1 = <<2:(8*Size)>>,
    Data2 = <<3:(8*Size)>>,
    Data3 = <<4:(8*Size)>>,
    A0 = low:write_leaf(Data0),
    Data0 = low:read_leaf(A0),
    A1 = low:write_leaf(Data1),
    Data1 = low:read_leaf(A1),
    low:delete_thing(A0, leaf_db),
    A0 = low:write_leaf(Data1),
    Data1 = low:read_leaf(A0),
    A2 = low:write_leaf(Data2),
    Data1 = low:read_leaf(A0),
    A3 = low:write_leaf(Data3),
    Data1 = low:read_leaf(A0),
    Data1 = low:read_leaf(A1),
    Data2 = low:read_leaf(A2),
    Data3 = low:read_leaf(A3),

    Size2 = low:word(stem_db),
    Data02 = <<11:(8*Size2)>>,
    Data12 = <<2:(8*Size2)>>,
    Data22 = <<3:(8*Size2)>>,
    Data32 = <<4:(8*Size2)>>,
    A02 = low:write_stem(Data02),
    A12 = low:write_stem(Data12),
    A22 = low:write_stem(Data22),
    A32 = low:write_stem(Data32),
    Data02 = low:read_stem(A02),
    Data12 = low:read_stem(A12),
    Data22 = low:read_stem(A22),
    Data32 = low:read_stem(A32),
    low:delete_thing(A02, stem_db),
    success.
