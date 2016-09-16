-module(trie).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, cfg/1,get/3,put/4,garbage/2,garbage_leaves/2]).
init(CFG) -> 
    StemID = ids:stem(CFG),
    ReplaceStem = <<0:(8*(dump:word(StemID)))>>,
    dump:put(ReplaceStem, StemID),
    {ok, CFG}.
start_link(CFG) -> %keylength, or M is the size outputed by hash:doit(_). 
    gen_server:start_link({global, ids:main(CFG)}, ?MODULE, CFG, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Value, Root, Weight}, _From, CFG) -> 
    ID = cfg:id(CFG),
    Key = dump_bits:top(ID),
    Leaf = leaf:new(Key, Weight, Value), 
    {_, NewRoot, _} = store:store(Leaf, Root, CFG),
    dump_bits:write(ID),
    {reply, {Key, NewRoot}, CFG};
handle_call({get, Key, Root}, _From, CFG) -> 
    P = leaf:path_maker(Key, CFG),
    {RootHash, Leaf, Proof} = get:get(P, Root, CFG),
    {reply, {RootHash, Leaf, Proof}, CFG};
handle_call({garbage, Keepers}, _From, CFG) -> 
    io:fwrite("gabage 2\n"),
    garbage:garbage(Keepers, CFG),
    {reply, ok, CFG};
handle_call({garbage_leaves, KLS}, _From, CFG) -> 
    garbage:garbage_leaves(KLS, CFG),
    {reply, ok, CFG};
handle_call(cfg, _From, CFG) ->
    {reply, CFG, CFG}.
cfg(ID) when is_atom(ID) -> gen_server:call({global, ids:main_id(ID)}, cfg).
put(Value, Root, Weight, CFG) ->
    gen_server:call({global, ids:main(CFG)}, {put, Value, Root, Weight}).
get(Key, Root, CFG) -> gen_server:call({global, ids:main(CFG)}, {get, Key, Root}).
garbage(Keepers, CFG) -> 
    io:fwrite("trie garbage \n"),
    gen_server:call({global, ids:main(CFG)}, {garbage, Keepers}).
garbage_leaves(KLS, CFG) ->
    gen_server:call({global, ids:main(CFG)}, {garbage_leaves, KLS}).

