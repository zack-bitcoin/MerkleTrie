-module(trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, root_hash/2,cfg/1,get/3,put/5,delete/3,garbage/2,garbage_leaves/2,get_all/2]).
init(CFG) -> 
    StemID = ids:stem(CFG),
    ReplaceStem = <<0:(8*(dump:word(StemID)))>>,
    0 = dump:put(ReplaceStem, StemID),
    {ok, CFG}.
start_link(CFG) -> %keylength, or M is the size outputed by hash:doit(_). 
    gen_server:start_link({global, ids:main(CFG)}, ?MODULE, CFG, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({garbage, Keepers}, CFG) -> 
    garbage:garbage(Keepers, CFG),
    {noreply, CFG};
handle_cast({garbage_leaves, KLS}, CFG) -> 
    garbage:garbage_leaves(KLS, CFG),
    {noreply, CFG};
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Key, Root}, _From, CFG) ->
    NewRoot = delete:delete(Key, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({put, Key, Value, Meta, Root}, _From, CFG) -> 
    Leaf = leaf:new(Key, Value, Meta, CFG),
    {_, NewRoot, _} = store:store(Leaf, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({get, Key, RootPointer}, _From, CFG) -> 
    P = leaf:path_maker(Key, CFG),
    {RootHash, L, Proof} = get:get(P, RootPointer, CFG),
    L2 = if
	     L == empty -> empty;
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
handle_call({garbage_leaves, KLS}, _From, CFG) -> 
    garbage:garbage_leaves(KLS, CFG),
    {reply, ok, CFG};
handle_call({root_hash, RootPointer}, _From, CFG) ->
    S = stem:get(RootPointer, CFG),
    H = stem:hash(S, CFG),
    {reply, H, CFG};
handle_call(cfg, _From, CFG) ->
    {reply, CFG, CFG}.
cfg(ID) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, cfg).
root_hash(ID, RootPointer) when is_atom(ID) ->
    gen_server:call({global, ids:main_id(ID)}, {root_hash, RootPointer}).
put(Key, Value, Meta, Root, ID) ->
    gen_server:call({global, ids:main_id(ID)}, {put, Key, Value, Meta, Root}).
get(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get, Key, Root}).
get_all(Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get_all, Root}).
delete(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {delete, Key, Root}).
garbage(Keepers, ID) -> 
    %io:fwrite("trie garbage \n"),
    gen_server:cast({global, ids:main_id(ID)}, {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:cast({global, ids:main_id(ID)}, {garbage_leaves, KLS}).


get_all_internal(Root, CFG) ->
    S = stem:get(Root, CFG),
    P = tuple_to_list(stem:pointers(S)),
    T = tuple_to_list(stem:types(S)),
    get_all_internal2(P, T, CFG).
get_all_internal2([], [], _) -> [];
get_all_internal2([A|AT], [T|TT], CFG) -> 
    B = case T of
	    0 -> %empty
		[];
	    1 -> %another stem
		get_all_internal(A, CFG);
	    2 -> %a leaf.
		[leaf:get(A, CFG)]
	end,
    B++get_all_internal2(AT, TT, CFG).
