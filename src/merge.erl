-module(merge).
-export([doit/4]).

doit(L, H, R, ID) -> 
    %first, in ram, generate the proof trie for the new data to calculate the new state root.
    %then, starting from the leaves, put the new trie onto the database, Only replace existing stems and leaves, don't add new stems or leaves.
    io:fwrite("R is "),
    io:fwrite(integer_to_list(R)),
    io:fwrite("\n"),
    Root = dump:get(R, ids:stem(ID)),
    H0 = stem:hash(Root),
    {H2, R2} = helper(L, H0, R, ID),
    H = H2,
    R2.
helper([], H, R, _) -> {H, R};
helper([{Path, Value, _Proof}|T], _H, R, ID) -> 
    io:fwrite("working here"),
    {H2, R2, _} = store:store(Path, Value, R, ID),
    helper(T, H2, R2, ID).

    
