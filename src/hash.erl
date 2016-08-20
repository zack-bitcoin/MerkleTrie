-module(hash).
-export([doit/1,test/0,hash/1]).

hash(S) -> crypto:hmac(sha256, S, "").
doit(S) when not(is_binary(S)) -> doit(term_to_binary(S));
doit(S) -> hash(S).
-record(p, {p = ""}).
test() -> 
    doit(123),
    doit(abc),
    doit([123]),
    doit([[[]]]),
    doit(#p{}),
    success.
