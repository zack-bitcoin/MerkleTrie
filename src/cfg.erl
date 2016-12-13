-module(cfg).
-compile(export_all).
-record(cfg, {weight, path, value, id}). 
new(P, V, ID) -> #cfg{path = P, value = V, id = ID}.
path(X) -> X#cfg.path. %how many bytes to store the path (defaul is 5)
value(X) -> X#cfg.value.%how many bytes to store the value.
leaf(X) -> path(X) + value(X).%eventually we will remove weight from here.
id(X) -> X#cfg.id.
