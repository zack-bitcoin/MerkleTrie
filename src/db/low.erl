-module(low).
-export([word/1,write_leaf/1,write_stem/1,read_leaf/1,read_stem/1,delete_thing/2,highest/1]).

word(Id) -> gen_server:call({global, Id}, word).
highest(Id) -> gen_server:call({global, Id}, highest).
write_leaf(Data) ->
    D = word(leaf_db),
    D = size(Data),
    gen_server:call({global, leaf_db}, {write, Data, leaf_bits}).
write_stem(Data) when is_binary(Data) ->
    D = size(Data),
    D = word(stem_db),
    gen_server:call({global, stem_db}, {write, Data, stem_bits});
write_stem(Data) ->
    write_stem(stem:serialize(Data)).
read_leaf(A) -> read_thing(A,leaf_db).
read_stem(A) -> read_thing(A,stem_db).
read_thing(A, ID) -> 
    case gen_server:call({global, ID}, {read, A}) of
	{error, _} -> error;
	X -> 
	    case ID of
		stem_db -> stem:deserialize(X);
		leaf_db -> X
	    end
    end.
delete_thing(A, leaf_db) -> 
    gen_server:cast({global, leaf_db}, {delete, A, leaf_bits});
delete_thing(A, stem_db) ->
    gen_server:cast({global, stem_db}, {delete, A, stem_bits}).

