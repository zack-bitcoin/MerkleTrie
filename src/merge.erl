-module(merge).
-export([doit/4]).

doit(L, H, R, CFG) -> 
    %first, in ram, generate the proof trie for the new data to calculate the new state root.
    %then, starting from the leaves, put the new trie onto the database, Only replace existing stems and leaves, don't add new stems or leaves.
    Root = dump:get(R, ids:stem(CFG)),
    H0 = stem:hash(Root, CFG),
    {H2, R2} = helper(L, H0, R, CFG),
    H = H2,
    R2.
helper([], H, R, _) -> {H, R};
helper([Leaf|T], _H, R, CFG) -> 
    {H2, R2, _} = store:store(Leaf, R, CFG),
    helper(T, H2, R2, CFG).

    
