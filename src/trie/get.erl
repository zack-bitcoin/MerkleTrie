-module(get).
-export([get/2]).

get(Path, Root) -> %returns {RootHash, Value, Proof}
    <<X:4, Y:4, _/binary>> = Path,
    S = low:read_stem(Root),
    H = stem:hash(S),
    {A, B} = get2(Path, S, [stem:hashes(S)]),
    <<C:256, D/binary>> = A,
    {H, <<C:256>>, D, B}.
get2(<<N:4, Path/bitstring>>, Stem, Proof) ->
    P = stem:pointers(Stem),
    T = stem:types(Stem),
    NextType = element(N+1, T),
    case NextType of
	1 -> %another stem
	    Next = low:read_stem(element(N+1, P)), 
	    get2(Path, Next, [stem:hashes(Next)|Proof]);
	2 -> %leaf
	    {low:read_leaf(element(N+1, P)), Proof}
    end.
