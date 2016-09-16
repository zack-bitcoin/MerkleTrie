-module(trie).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, s/1,ws/1,get/3,put/4,garbage/2,garbage_leaves/2,m/1]).
init([Size, KeyLength, ID, WS]) -> 
    StemID = ids:stem(ID),
    ReplaceStem = <<0:(8*(dump:word(StemID)))>>,
    dump:put(ReplaceStem, StemID),
    {ok, {Size, KeyLength, ID, WS}}.
start_link(Size, KeyLength, ID, WS) -> %keylength, or M is the size outputed by hash:doit(_). 
    gen_server:start_link({global, ids:main(ID)}, ?MODULE, [Size, KeyLength, ID, WS], []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Value, Root, Weight}, _From, {Size, M, ID, WS}) -> 
    Key = dump_bits:top(ID),
    Leaf = leaf:new(Key, Weight, Value), 
    {_, NewRoot, _} = store:store(Leaf, Root, ID, WS, Size+M+WS, M),
    dump_bits:write(ID),
    {reply, {Key, NewRoot}, {Size, M, ID, WS}};
handle_call({get, Key, Root}, _From, {Size, M, ID, WS}) -> 
    P = leaf:path_maker(Key, M),
    {RootHash, Leaf, Proof} = get:get(P, Root, ID, WS, Size+M+WS),
    {reply, {RootHash, leaf:value(Leaf), leaf:weight(Leaf), Proof}, {Size, M, ID, WS}};
handle_call({garbage, Keepers}, _From, {Size, M, ID, WS}) -> 
    io:fwrite("gabage 2\n"),
    garbage:garbage(Keepers, M, ID, WS, M+Size+WS),
    {reply, ok, {Size, M, ID, WS}};
handle_call({garbage_leaves, KLS}, _From, {Size, M, ID, WS}) -> 
    garbage:garbage_leaves(KLS, M, ID, WS, M+Size+WS),
    {reply, ok, {Size, M, ID, WS}};
handle_call(ws, _From, {Size, M, ID, WS}) -> 
    {reply, WS, {Size, M, ID, WS}};
handle_call(m, _From, {Size, M, ID, WS}) -> 
    {reply, M, {Size, M, ID, WS}};
handle_call(s, _From, {Size, M, ID, WS}) -> 
    {reply, Size, {Size, M, ID, WS}}.
ws(ID) -> gen_server:call({global, ids:main(ID)}, ws).
put(Value, Root, Weight, ID) ->
    gen_server:call({global, ids:main(ID)}, {put, Value, Root, Weight}).
get(Key, Root, ID) -> gen_server:call({global, ids:main(ID)}, {get, Key, Root}).
m(ID) -> gen_server:call({global, ids:main(ID)}, m).
s(ID) -> gen_server:call({global, ids:main(ID)}, s).
garbage(Keepers, ID) -> 
    io:fwrite("trie garbage \n"),
    gen_server:call({global, ids:main(ID)}, {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:call({global, ids:main(ID)}, {garbage_leaves, KLS}).

