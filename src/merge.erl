-module(merge).
-export([doit/3]).

doit(L, H, R) -> 
    %first, in ram, generate the proof trie for the new data to calculate the new state root.
    %then, starting from the leaves, put the new trie onto the database, Only replace existing stems and leaves, don't add new stems or leaves.
    io:fwrite("R is "),
    io:fwrite(integer_to_list(R)),
    io:fwrite("\n"),
    Root = dump:get(R, stem),
    H0 = stem:hash(Root),
    {H2, R2} = helper(L, H0, R),
    H = H2,
    R2.
helper([], H, R) -> {H, R};
helper([{Path, Value, _Proof}|T], _H, R) -> 
    io:fwrite("working here"),
    {H2, R2, _} = store:store(Path, Value, R),
    helper(T, H2, R2).

    
