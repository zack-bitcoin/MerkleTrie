-module(cfg).
-compile(export_all).
-record(cfg, {path, value, id, meta, hash_size}). 
new(P, V, ID, M, H) -> #cfg{path = P, value = V, 
			    id = ID, meta = M,
			    hash_size = H }.
path(X) -> X#cfg.path. %how many bytes to store the path (defaul is 5)
value(X) -> X#cfg.value.%how many bytes to store the value.
meta(X) -> X#cfg.meta. %how many bytes to store the meta data that isn't hashed into the merkle tree.
leaf(X) -> path(X) + value(X) + meta(X).
id(X) -> X#cfg.id.
hash_size(X) -> X#cfg.hash_size.
