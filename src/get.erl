-module(get).
-export([get/3]).

get(Path, Root, ID) -> %returns {RootHash, Path, Value, Proof}
    S = stem:deserialize(dump:get(Root, ids:stem(ID))),
    H = stem:hash(S),
    case get2(Path, S, [stem:hashes(S)], ID) of
	{A, B} ->
	    <<Path2:40, Value/binary>> = A,
	    {H, <<Path2:40>>, Value, B};
	empty ->
	    empty
    end.       
get2(<<N:4, Path/bitstring>>, Stem, Proof, ID) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    empty;
	1 -> %another stem
	    Next = stem:deserialize(dump:get(PN, ids:stem(ID))), 
	    get2(Path, Next, [stem:hashes(Next)|Proof], ID);
	2 -> %leaf
	    {dump:get(PN, ids:leaf(ID)), Proof}
    end.
