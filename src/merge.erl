-module(merge).
-export([doit/7]).

doit(L, H, R, WS, LS, M, ID) -> 
    %first, in ram, generate the proof trie for the new data to calculate the new state root.
    %then, starting from the leaves, put the new trie onto the database, Only replace existing stems and leaves, don't add new stems or leaves.
    Root = dump:get(R, ids:stem(ID)),
    H0 = stem:hash(Root, WS),
    {H2, R2} = helper(L, H0, R, ID, WS, LS, M),
    H = H2,
    R2.
helper([], H, R, _, _, _, _) -> {H, R};
helper([Leaf|T], _H, R, ID, WS, LS, M) -> 
    {H2, R2, _} = store:store(Leaf, R, ID, WS, LS, M),
    helper(T, H2, R2, ID, WS, LS, M).

    
