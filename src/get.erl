-module(get).
-export([get/4]).

get(Path, Root, ID, WS) -> %returns {RootHash, Path, Value, Proof}
    S = stem:deserialize(dump:get(Root, ids:stem(ID)), WS),
    H = stem:hash(S, WS),
    case get2(Path, S, [stem:hashes(S)], ID, WS) of
	{A, Proof} ->
	    <<Weight:WS, Path2:40, Value/binary>> = A,
	    {H, <<Path2:40>>, Value, Proof, Weight};
	empty ->
	    empty
    end.       
get2(<<N:4, Path/bitstring>>, Stem, Proof, ID, WS) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    empty;
	1 -> %another stem
	    Next = stem:deserialize(dump:get(PN, ids:stem(ID)), WS), 
	    get2(Path, Next, [stem:hashes(Next)|Proof], ID, WS);
	2 -> %leaf
	    {dump:get(PN, ids:leaf(ID)), Proof}
    end.
