-module(trie).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, get/2,put/3,garbage/1,garbage_leaves/1,m/0]).
-define(M, gen_server:call(?MODULE, m)).
init([Size, Max]) ->  
    ReplaceStem = <<0:(8*(dump:word(stem)))>>,
    dump:put(ReplaceStem, stem),
    {ok, {Size, Max}}.
start_link(Size, Max) -> 
    M = round(math:log(Max) / math:log(2)),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size, M], []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Key, Value, Root}, _From, {Size, M}) -> 
    P = hash:doit(Key),
    %M = size(Key),
    Size = size(Value),
    {_, NewRoot, _} = store:store(P, Value, Root),
    {reply, NewRoot, {Size, M}};
handle_call({get, Key, Root}, _From, {Size, M}) -> 
    P = hash:doit(Key),
    {RootHash, P, Value, Proof} = get:get(P, Root),
    {reply, {RootHash, Value, Proof}, {Size, M}};
handle_call({garbage, Keepers}, _From, X) -> 
    garbage:garbage(Keepers),
    {reply, ok, X};
handle_call({garbage_leaves, KLS}, _From, X) -> 
    garbage:garbage_leaves(KLS),
    {reply, ok, X};
handle_call(m, _From, {Size, M}) -> 
    {reply, M, {Size, M}};
handle_call(_, _From, X) -> {reply, X, X}.

put(Key, Value, Root) ->
    gen_server:call(?MODULE, {put, Key, Value, Root}).
get(Key, Root) -> gen_server:call(?MODULE, {get, Key, Root}).
m() -> ?M.%gen_server:call(?MODULE, m).
garbage(Keepers) -> gen_server:call(?MODULE, {garbage, Keepers}).
garbage_leaves(KLS) ->
    gen_server:call(?MODULE, {garbage_leaves, KLS}).
