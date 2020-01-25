-module(trie).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 root_hash/2,cfg/1,get/3,
	 put/5,put_batch/3,delete/3,%garbage/2,garbage_leaves/2,
	 get_all/2,new_trie/2, restore/5,restore/7, 
	 empty/1, 
	 quick_save/1, %the copy of the ets currently in ram, it uses this to update the copy stored on the hard drive.
	 reload_ets/1, %grabs the copy of the ets from the hard drive, loads it into ram
	 clean_ets/2, %deletes everything from the merkel tree database, except for what can be proved from this single state root.
	 garbage/3]).
-record(d, {m, l, empty}).
init({CFG, Loc}) ->
    process_flag(trap_exit, true),
    D = case db:read(mtree:loc2rest(Loc)) of
	    "" ->
		M = new_m(CFG),
		#d{m = M, l = Loc, empty = 1};
	    Y ->
		M = binary_to_term(Y),
		{ok, P} = ets:file2tab(Loc),
		M2 = mtree:set_ets(M, P),
		#d{m = M2, l = Loc, empty = 1}
	end,
    {ok, D}.
new_m(CFG) ->
    M = mtree:new_empty(cfg:path(CFG), 
			cfg:value(CFG),
			cfg:meta(CFG)).
    
start_link(CFG, Location) -> %keylength, or M is the size outputed by hash:doit(_). 
    gen_server:start_link({global, ids:main(CFG)}, ?MODULE, {CFG, Location}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, D) -> 
    mtree:save_to_file(D#d.m, D#d.l),
    io:fwrite("tree "), 
    %io:fwrite(D#d.l),
    %io:fwrite(" "),
    %CFG = mtree:cfg(D#d.m),
    %ID = cfg:id(CFG),
    %io:fwrite(ID),
    io:fwrite(" died \n"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(reload_ets, D) -> 
    M = mtree:load_from_file(D#d.l),
    %CFG = mtree:cfg(D#d.m),
    %M = new_m(CFG),
    D2 = D#d{m = M},
    {noreply, D2};
handle_cast(_, X) -> {noreply, X}.
handle_call(quick_save, _, D) -> 
    mtree:save_to_file(D#d.m, D#d.l),
    {reply, ok, D};
handle_call({clean_ets, Pointer}, _, D) -> 
    M = D#d.m,
    M2 = mtree:clean(M, Pointer),
    D2 = D#d{m = M2},
    {reply, ok, D2};
handle_call({garbage, NewRoot, OldRoot}, _From, D) ->%prune new
    %deletes newroot
    X = mtree:garbage(NewRoot, OldRoot, D#d.m),
    {reply, X, D};
handle_call({restore, Key, Value, Meta, Hash, Proof, Root}, _From, D) -> 
    valid_key(Key),
    M = D#d.m,
    CFG = mtree:cfg(M),
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {Hash, NewRoot, _Proof, M2} = mtree:restore(Leaf, Hash, Proof, Root, M),
    D2 = D#d{m = M2},
    {reply, NewRoot, D2};
handle_call({put, Key, Value, Meta, Root}, _From, D) -> 
    valid_key(Key),
    CFG = mtree:cfg(D#d.m),
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {NewRoot, M2} = mtree:store_batch([Leaf], Root, D#d.m),
    {reply, NewRoot, D#d{m = M2}};
handle_call({put_batch, Leaves, Root}, _From, D) ->
    {NewRoot, M2} = mtree:store_batch(Leaves, Root, D#d.m),
    {reply, NewRoot, D#d{m = M2}};
handle_call({get, Key, RootPointer}, _From, D) -> 
    valid_key(Key),
    M = D#d.m,
    CFG = mtree:cfg(M),
    P = leaf:path_maker(Key, CFG),
    {RootHash, L, Proof} = mtree:get(P, RootPointer, M),
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
    {reply, {RootHash, L2, Proof}, D};
handle_call({get_all, Root}, _From, D) ->
    X = mtree:get_all(Root, D#d.m),
    {reply, X, D};
handle_call(empty, _, D) ->
    {reply, D#d.empty, D};
handle_call({new_trie, RootStem}, _From, D) ->
    CFG = mtree:cfg(D#d.m),
    {M, P} = mtree:new_restoration(RootStem, cfg:path(CFG), cfg:value(CFG), cfg:meta(CFG)),
    {reply, {M, P}, D};
handle_call({root_hash, RootPointer}, _From, D) ->
    H = mtree:root_hash(RootPointer, D#d.m),
    {reply, H, D};
handle_call(cfg, _From, D) ->
    CFG = mtree:cfg(D#d.m),
    {reply, CFG, D}.

cfg(ID) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, cfg).
new_trie(ID, RootStem) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, {new_trie, RootStem}).
clean_ets(ID, Pointer) ->
    %deletes everything from the merkel tree database, except for what can be proved from this single state root.
    %used for loading a checkpoint.
    gen_server:call({global, ids:main_id(ID)}, {clean_ets, Pointer}).
    
reload_ets(ID) ->
    %reloads the ram databases from the hard drive copy.
    gen_server:cast({global, ids:main_id(ID)}, reload_ets).
quick_save(ID) ->
    gen_server:call({global, ids:main_id(ID)}, quick_save).
empty(ID) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, empty).
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

valid_key(Key) ->
    true = is_integer(Key),
    true = Key > 0.
    
