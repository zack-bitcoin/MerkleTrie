-module(random_get).
-export([get/3]).

get(Seed, Root, CFG) ->
    HD = trie_hash:hash_depth() * 8,
    <<P:HD>> = trie_hash:doit(Seed),
    S = stem:get(Root, CFG),
    TotalWeight = stem:weight(S),
    %io:fwrite(integer_to_list(TotalWeight)),
    Q = P rem TotalWeight,
    H = stem:hash(S, CFG),
    {Leaf, Proof} = get2(Q, S, [stem:hashes(S)], CFG),
    {H, Leaf, Proof}.
    
get2(Select, Stem, Proof, CFG) ->
    Weights = stem:weights(Stem),
    {N, NewSelect} = find_n(Select, Weights),
    NextType = stem:type(N+1, Stem),
    Pointer = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    {error, 0, "stems say incorrect weights on branches"};
	1 -> 
	    NewStem = stem:get(Pointer, CFG),
	    get2(NewSelect, NewStem, [stem:hashes(NewStem)|Proof], CFG);
	2 ->
	    Leaf = leaf:get(Pointer, CFG),
	    {Leaf, Proof}
    end.

find_n(Select, Weights) ->
   W = tuple_to_list(Weights), 
   find_n2(Select, W, 0).
find_n2(Select, [A], 15) ->
    true = Select =< A,
    {15, Select}; 
find_n2(Select, [A|[B|C]], N) ->
    if
	A > Select ->
	    {N, Select};
	true ->
	    find_n2(Select - A, [B|C], N+1)
    end.
