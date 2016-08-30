-module(get).
-export([get/2]).

get(Path, Root) -> %returns {RootHash, Path, Value, Proof}
    S = stem:deserialize(dump:get(Root, stem)),
    H = stem:hash(S),
    case get2(Path, S, [stem:hashes(S)]) of
	{A, B} ->
	    <<Path2:96, Value/binary>> = A,
	    {H, <<Path2:96>>, Value, B};
	empty ->
	    empty
    end.       
get2(<<N:4, Path/bitstring>>, Stem, Proof) ->
    NextType = stem:type(N+1, Stem),
    PN = stem:pointer(N+1, Stem),
    case NextType of
	0 -> %empty
	    empty;
	1 -> %another stem
	    Next = stem:deserialize(dump:get(PN, stem)), 
	    get2(Path, Next, [stem:hashes(Next)|Proof]);
	2 -> %leaf
	    {dump:get(PN, leaf), Proof}
    end.
