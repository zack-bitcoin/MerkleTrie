-module(get).
-export([get/2]).

get(Path, Root) -> %returns {RootHash, Value, Proof}
    %<<X:4, Y:4, _/binary>> = Path,
    S = dump:get(Root, stem),
    H = stem:hash(S),
    {A, B} = get2(Path, S, [stem:hashes(S)]),
    <<C:256, D/binary>> = A,
    {H, <<C:256>>, D, B}.
get2(<<N:4, Path/bitstring>>, Stem, Proof) ->
    P = stem:pointers(Stem),
    T = stem:types(Stem),
    NextType = element(N+1, T),
    PN = element(N+1, P),
    case NextType of
	1 -> %another stem
	    Next = dump:get(PN, stem), 
	    get2(Path, Next, [stem:hashes(Next)|Proof]);
	2 -> %leaf
	    {dump:read_leaf(PN, leaf), Proof}
    end.
