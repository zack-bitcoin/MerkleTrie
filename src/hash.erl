-module(hash).
-export([doit/1,hash/1]).

hash(S) -> crypto:hmac(sha256, S, "").
doit(S) when not(is_binary(S)) -> doit(term_to_binary(S));
doit(S) -> hash(S).
