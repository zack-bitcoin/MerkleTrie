-module(merge).
-export([doit/3]).

doit(H, L, R) -> 
    {H, R2} = helper(L, R),
    R2.
helper([], R) -> R;
helper([{Path, Value, Proof}|T], R) -> 
    io:fwrite("working here"),
    R2 = ok,
    helper(T, R2).

    
