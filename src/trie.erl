-module(trie).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, get/2,put/2,garbage/1,garbage_leaves/1,m/0,integer2path/2,flip_bytes/1,to_path/1]).
-define(M, gen_server:call(?MODULE, m)).
init([Size, Max]) ->  
    ReplaceStem = <<0:(8*(dump:word(stem)))>>,
    dump:put(ReplaceStem, stem),
    {ok, {Size, Max}}.
start_link(Size, Max) -> 
    M = round((math:log(Max) / math:log(2) / 8) + 0.5)*8,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size, M], []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Value, Root}, _From, {Size, M}) -> 
    %P = hash:doit(Key),
    Key = dump_bits:top(trie),
    P = integer2path(Key, M),
    Size = size(Value),
    {_, NewRoot, _} = store:store(P, Value, Root),
    dump_bits:write(trie),
    {reply, {Key, NewRoot}, {Size, M}};
handle_call({get, Key, Root}, _From, {Size, M}) -> 
    %P = hash:doit(Key),
    P = integer2path(Key, M),
    {RootHash, P, Value, Proof} = get:get(P, Root),
    {reply, {RootHash, Value, Proof}, {Size, M}};
handle_call({garbage, Keepers}, _From, {Size, M}) -> 
    garbage:garbage(Keepers, M),
    {reply, ok, {Size, M}};
handle_call({garbage_leaves, KLS}, _From, {Size, M}) -> 
    garbage:garbage_leaves(KLS, M),
    {reply, ok, {Size, M}};
handle_call(m, _From, {Size, M}) -> 
    {reply, M, {Size, M}};
handle_call({to_path, X}, _From, {Size, M}) -> 
    Y = integer2path(X, M),
    {reply, Y, {Size, M}};
handle_call(_, _From, X) -> {reply, X, X}.

put(Value, Root) ->
    gen_server:call(?MODULE, {put, Value, Root}).
get(Key, Root) -> gen_server:call(?MODULE, {get, Key, Root}).
m() -> ?M.%gen_server:call(?MODULE, m).
garbage(Keepers) -> gen_server:call(?MODULE, {garbage, Keepers}).
garbage_leaves(KLS) ->
    gen_server:call(?MODULE, {garbage_leaves, KLS}).
integer2path(I, M) -> flip_bytes(<<I:M>>).
flip_bytes(X) -> flip_bytes(X, <<>>).
flip_bytes(<<>>, X) -> X;
flip_bytes(<<N:8, T/bitstring>>, X) -> 
    flip_bytes(T, <<N:8, X/bitstring>>).
to_path(X) ->
    gen_server:call(?MODULE, {to_path, X}).

