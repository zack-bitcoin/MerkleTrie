-module(trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, root_hash/2,cfg/1,get/3,put/5,put_batch/3,delete/3,%garbage/2,garbage_leaves/2,
	 get_all/2,new_trie/2, restore/5,restore/7, 
	 prune/3, garbage/3]).
init(CFG) ->
    process_flag(trap_exit, true),
    ID = cfg:id(CFG),
    Top = case cfg:mode(CFG) of
	      hd -> bits:top(ID);
	      ram -> dump:highest(ids:stem(CFG))
	  end,
    case Top of
	1 -> stem:put(stem:new_empty(CFG), CFG);%doesn't return 1 when we restart.
	_ -> ok
    end,
    {ok, CFG}.
start_link(CFG) -> %keylength, or M is the size outputed by hash:doit(_). 
    gen_server:start_link({global, ids:main(CFG)}, ?MODULE, CFG, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, CFG) -> 
    io:fwrite("tree "), 
    ID = cfg:id(CFG),
    io:fwrite(ID),
    io:fwrite(" died \n"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({garbage, NewRoot, OldRoot}, _From, CFG) ->%prune new
    X = prune:garbage(NewRoot, OldRoot, CFG),
    {reply, X, CFG};
handle_call({prune, OldRoot, NewRoot}, _From, CFG) ->%prune old
    1=2,
    X = prune:stem(OldRoot, NewRoot, CFG),
    {reply, X, CFG};
handle_call({delete, Key, Root}, _From, CFG) ->
    valid_key(Key),
    NewRoot = delete:delete(Key, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({restore, Key, Value, Meta, Hash, Proof, Root}, _From, CFG) -> 
    valid_key(Key),
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {Hash, NewRoot, _} = store:restore(Leaf, Hash, Proof, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({put, Key, Value, Meta, Root}, _From, CFG) -> 
    valid_key(Key),
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {_, NewRoot, _} = store:store(Leaf, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({put_batch, Leaves, Root}, _From, CFG) ->
    {Hash, NewRoot} = store:batch(Leaves, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({get, Key, RootPointer}, _From, CFG) -> 
    valid_key(Key),
    P = leaf:path_maker(Key, CFG),
    {RootHash, L, Proof} = get:get(P, RootPointer, CFG),
    L2 = if
	     L == empty -> empty;
	     L == unknown -> unknown;
	     true ->
		 Key2 = leaf:key(L),
		 if
		     Key == Key2 -> L;
		     true -> empty
		 end
	 end,
    {reply, {RootHash, L2, Proof}, CFG};
handle_call({get_all, Root}, _From, CFG) ->
    X = get_all_internal(Root, CFG),
    {reply, X, CFG};
handle_call({new_trie, RootStem}, _From, CFG) ->
    %Stem = stem:empty_trie(Root, CFG),
    Stem = stem:update_pointers(RootStem, stem:empty_tuple()),
    X = stem:put(Stem, CFG),
    {reply, X, CFG};
handle_call({root_hash, RootPointer}, _From, CFG) ->
    S = stem:get(RootPointer, CFG),
    H = stem:hash(S, CFG),
    {reply, H, CFG};
handle_call(cfg, _From, CFG) ->
    {reply, CFG, CFG}.
cfg(ID) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, cfg).
new_trie(ID, RootStem) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, {new_trie, RootStem}).
-spec root_hash(atom(), stem:stem_p()) -> stem:hash().
root_hash(ID, RootPointer) when (is_atom(ID) and is_integer(RootPointer))->
    gen_server:call({global, ids:main_id(ID)}, {root_hash, RootPointer}).
-spec put(leaf:key(), leaf:value(), leaf:meta(), stem:stem_p(), atom()) ->
		 stem:stem_p().
restore(Leaf, Hash, Path, Root, ID) ->
    restore(leaf:key(Leaf), leaf:value(Leaf), leaf:meta(Leaf),
	    Hash, Path, Root, ID).
restore(Key, Value, Meta, Hash, Path, Root, ID) ->
    gen_server:call({global, ids:main_id(ID)}, {restore, Key, Value, Meta, Hash, Path, Root}).
put_batch([], Root, _) -> Root;
put_batch(Leaves, Root, ID) -> 
    gen_server:call({global, ids:main_id(ID)}, {put_batch, Leaves, Root}).
put(Key, Value, Meta, Root, ID) ->
    gen_server:call({global, ids:main_id(ID)}, {put, Key, Value, Meta, Root}).
-spec get(leaf:key(), stem:stem_p(), atom()) ->
		 {stem:hash(), empty | leaf:leaf(), get:proof()}.
get(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get, Key, Root}).
-spec get_all(stem:stem_p(), atom()) -> [leaf:leaf()].
get_all(Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get_all, Root}).
-spec delete(leaf:key(), stem:stem_p(), atom()) -> stem:stem_p().
delete(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {delete, Key, Root}).
garbage(NewRoot, OldRoot, ID) ->%removes new
    gen_server:call({global, ids:main_id(ID)}, {garbage, NewRoot, OldRoot}).
prune(OldRoot, NewRoot, ID) ->%removes old
    gen_server:call({global, ids:main_id(ID)}, {prune, OldRoot, NewRoot}).

get_all_internal(Root, CFG) ->
    S = stem:get(Root, CFG),
    P = tuple_to_list(stem:pointers(S)),
    T = tuple_to_list(stem:types(S)),
    get_all_internal2(P, T, CFG).
get_all_internal2([], [], _) -> [];
get_all_internal2([A|AT], [T|TT], CFG) -> 
    B = case T of
	    0 -> [];%empty
	    1 -> get_all_internal(A, CFG);%another stem
	    2 -> [leaf:get(A, CFG)]%a leaf
	end,
    B++get_all_internal2(AT, TT, CFG).
valid_key(Key) ->
    true = is_integer(Key),
    true = Key > 0.
    
