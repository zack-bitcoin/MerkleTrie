-module(hash).
-export([doit/1,hash/1]).

hash(S) -> 
    HD = merkle_constants:hash_depth(),
    <<X:HD, _/bitstring>> = crypto:hmac(sha256, S, ""),
    <<X:HD>>.
doit(S) when not(is_binary(S)) -> doit(term_to_binary(S));
doit(S) -> hash(S).
