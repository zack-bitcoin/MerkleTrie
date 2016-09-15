-module(get).
-export([get/5]).

get(Path, Root, ID, WS, LS) -> %returns {RootHash, Path, Value, Proof}
    S = stem:get(Root, WS, ID),
    H = stem:hash(S, WS),
    case get2(Path, S, [stem:hashes(S)], ID, WS, LS) of
	{A, Proof} -> {H, A, Proof};
	empty -> empty
    end.       
get2(<<N:4, Path/bitstring>>, Stem, Proof, ID, WS, LS) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    empty;
	1 -> %another stem
	    Next = stem:get(PN, WS, ID),
	    get2(Path, Next, [stem:hashes(Next)|Proof], ID, WS, LS);
	2 -> %leaf
	    Leaf2 = leaf:get(PN, WS, LS, ID),
	    {Leaf2, Proof}
    end.
