-module(config).
-export([]).
-record(cfg, {weight, path, value}).

new(W, P, V) -> #cfg{weight = W, path = P, value = V}.
weight(X) -> X#cfg.weight.%how many bytes to store the weight
path(X) -> X#cfg.path. %how many bytes to store the path (defaul is 5)
value(X) -> X#cfg.value.%how many bytes to store the value.
leaf_size(X) -> weight(X) + path(X) + value(X).%eventually we will remove weight from here.
