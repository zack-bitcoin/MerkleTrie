-module(cfg).
-compile(export_all).
-export_type([cfg/0,path/0,value/0,id/0,meta/0,hash_size/0]).
-record(cfg, { path :: path()
	     , value :: value()
	     , id :: id()
	     , meta :: meta()
	     , hash_size :: hash_size()
	     }).
-opaque cfg() :: #cfg{}.
-type path() :: pos_integer().
-type value() :: non_neg_integer().
-type id() :: atom().
-type meta() :: non_neg_integer().
-type hash_size() :: pos_integer().
-spec new(path(), value(), id(), meta(), hash_size()) -> cfg().
new(P, V, ID, M, H) -> #cfg{path = P, value = V, 
			    id = ID, meta = M,
			    hash_size = H }.
-spec path(cfg()) -> path().
path(X) -> X#cfg.path. %how many bytes to store the path (defaul is 5)
-spec value(cfg()) -> value().
value(X) -> X#cfg.value.%how many bytes to store the value.
-spec meta(cfg()) -> meta().
meta(X) -> X#cfg.meta. %how many bytes to store the meta data that isn't hashed into the merkle tree.
leaf(X) -> path(X) + value(X) + meta(X).
-spec id(cfg()) -> id().
id(X) -> X#cfg.id.
-spec hash_size(cfg()) -> hash_size().
hash_size(X) -> X#cfg.hash_size.
