-module(cfg).
-compile(export_all).
-record(cfg, {weight, path, value, id, trie = stem:new_empty()}). 
new(W, P, V, ID) -> #cfg{weight = W, path = P, value = V, id = ID}.
weight(X) -> X#cfg.weight.%how many bytes to store the weight (The weighting is used for weighted random accesses)
path(X) -> X#cfg.path. %how many bytes to store the path (defaul is 5)
value(X) -> X#cfg.value.%how many bytes to store the value.
leaf(X) -> weight(X) + path(X) + value(X).%eventually we will remove weight from here.
id(X) -> X#cfg.id.
trie(X) -> X#cfg.trie. 
