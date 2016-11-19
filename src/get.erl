-module(get).
-export([get/3]).

get(Path, Root, CFG) -> %returns {RootHash, Path, Value, Proof}
    S = stem:get(Root, CFG),
    H = stem:hash(S, CFG),
    case get2(Path, S, [stem:hashes(S)], CFG) of
	{empty, Proof} -> {H, empty, Proof};
	{A, Proof} -> {H, A, Proof}
    end.       
get2(<<N:4, Path/bitstring>>, Stem, Proof, CFG) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    {empty, Proof};
	1 -> %another stem
	    Next = stem:get(PN, CFG),
	    get2(Path, Next, [stem:hashes(Next)|Proof], CFG);
	2 -> %leaf
	    Leaf2 = leaf:get(PN, CFG),
	    {Leaf2, Proof}
    end.
