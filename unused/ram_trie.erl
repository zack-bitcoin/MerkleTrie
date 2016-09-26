-module(ram_trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, cfg/1,get/3,put/4,garbage/2,garbage_leaves/2]).
init(CFG) -> {ok, CFG}.
start_link(CFG) -> gen_server:start_link({global, ids:ram(CFG)}, ?MODULE, CFG, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({garbage, Keepers}, CFG) ->
    ram_garbage:garbage(Keepers, CFG),
    {noreply, CFG};
handle_cast({garbage_leaves, KLS}, CFG) ->
    ram_garbage:garbage_leaves(KLS, CFG),
    {noreply, CFG};
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Value, Root, Weight}, _From, CFG) -> 
    ID = ids:ram(cfg:id(CFG)),
    Key = bits:top(ID),
    Leaf = leaf:new(Key, Weight, Value),
    {_, NewRoot, _} = ram_store:store(Leaf, Root, CFG),
    bits:write(ID),
    {reply, {Key, NewRoot}, CFG};
handle_call({get, Key, Root}, _From, CFG) ->
    P = leaf:path_maker(Key, CFG),
    {RootHash, Leaf, Proof} = ram_get:get(P, Root, CFG),
    {reply, {RootHash, Leaf, Proof}, CFG};
handle_call({garbage, Keepers}, _From, CFG) ->
    ram_garbage:garbage(Keepers, CFG),
    {reply, ok, CFG};
handle_call({garbage_leaves, KLS}, _From, CFG) ->
    ram_garbage:garbage_leaves(KLS, CFG),
    {reply, ok, CFG};
handle_call(cfg, _From, CFG) ->
    {reply, CFG, CFG}.
cfg(ID) when is_atom(ID) -> gen_server:call({global, ids:main_id(ID)}, cfg).
put(Value, Root, Weight, ID) ->
    gen_server:call({global, ids:main_id(ID)}, {put, Value, Root, Weight}).
get(Key, Root, ID) -> gen_server:call({global, ids:main_id(ID)}, {get, Key, Root}).
garbage(Keepers, ID) -> 
    io:fwrite("trie garbage \n"),
    gen_server:cast({global, ids:main_id(ID)}, {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:cast({global, ids:main_id(ID)}, {garbage_leaves, KLS}).


