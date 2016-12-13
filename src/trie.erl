-module(trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, root_hash/2,cfg/1,get/3,put/4,garbage/2,garbage_leaves/2]).
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
    io:fwrite("gabage 2\n"),
    garbage:garbage(Keepers, CFG),
    {noreply, CFG};
handle_cast({garbage_leaves, KLS}, CFG) -> 
    garbage:garbage_leaves(KLS, CFG),
    {noreply, CFG};
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Key, Value, Root}, _From, CFG) -> 
    Leaf = leaf:new(Key, Value, CFG),
    {_, NewRoot, _} = store:store(Leaf, Root, CFG),
    {reply, NewRoot, CFG};
handle_call({get, Key, RootPointer}, _From, CFG) -> 
    P = leaf:path_maker(Key, CFG),
    {RootHash, L, Proof} = get:get(P, RootPointer, CFG),
    {reply, {RootHash, L, Proof}, CFG};
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
put(Key, Value, Root, ID) ->
    gen_server:call({global, ids:main_id(ID)}, {put, Key, Value, Root}).
get(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get, Key, Root}).
garbage(Keepers, ID) -> 
    io:fwrite("trie garbage \n"),
    gen_server:cast({global, ids:main_id(ID)}, {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:cast({global, ids:main_id(ID)}, {garbage_leaves, KLS}).



