-module(hash).
-export([doit/1,hash/1,hash_depth/0]).

hash(S) -> 
    HD = hash_depth() * 8,
    <<X:HD, _/bitstring>> = crypto:hmac(sha256, S, ""),
    <<X:HD>>.
doit(S) when not(is_binary(S)) -> doit(term_to_binary(S));
doit(S) -> hash(S).
hash_depth() -> 12.
    
