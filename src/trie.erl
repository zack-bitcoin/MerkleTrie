-module(trie).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, get/3,put/4,garbage/2,garbage_leaves/2,m/1,integer2path/2,flip_bytes/1,to_path/2]).
-define(M(ID), gen_server:call({global, ids:main(ID)}, m)).
init([Size, KeyLength, ID, WS]) -> 
    StemID = ids:stem(ID),
    ReplaceStem = <<0:(8*(dump:word(StemID)))>>,
    dump:put(ReplaceStem, StemID),
    {ok, {Size, KeyLength, ID, WS}}.
start_link(Size, KeyLength, ID, WS) -> 
    %M = round((math:log(Max) / math:log(2) / 8) + 0.5)*8,
    gen_server:start_link({global, ids:main(ID)}, ?MODULE, [Size, KeyLength*8, ID, WS], []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({put, Value, Root, Weight}, _From, {Size, M, ID, WS}) -> 
    %P = hash:doit(Key),
    Key = dump_bits:top(ID),
    P = integer2path(Key, M),
    Size = size(Value),
    {_, NewRoot, _} = store:store(P, Value, Root, ID, WS, Weight),
    dump_bits:write(ID),
    {reply, {Key, NewRoot}, {Size, M, ID, WS}};
handle_call({get, Key, Root}, _From, {Size, M, ID, WS}) -> 
    %P = hash:doit(Key),
    P = integer2path(Key, M),
    {RootHash, P, Value, Proof, Weight} = get:get(P, Root, ID, WS),
    {reply, {RootHash, Value, Weight, Proof}, {Size, M, ID, WS}};
handle_call({garbage, Keepers}, _From, {Size, M, ID, WS}) -> 
    io:fwrite("gabage 2\n"),
    garbage:garbage(Keepers, M, ID, WS),
    {reply, ok, {Size, M, ID, WS}};
handle_call({garbage_leaves, KLS}, _From, {Size, M, ID, WS}) -> 
    garbage:garbage_leaves(KLS, M, ID, WS),
    {reply, ok, {Size, M, ID, WS}};
handle_call(m, _From, {Size, M, ID, WS}) -> 
    {reply, M, {Size, M, ID, WS}};
handle_call({to_path, X}, _From, {Size, M, ID, WS}) -> 
    Y = integer2path(X, M),
    {reply, Y, {Size, M, ID, WS}}.
%handle_call(_, _From, X) -> {reply, X, X}.

put(Value, Root, Weight, ID) ->
    gen_server:call({global, ids:main(ID)}, {put, Value, Root, Weight}).
get(Key, Root, ID) -> gen_server:call({global, ids:main(ID)}, {get, Key, Root}).
m(ID) -> ?M(ID).
garbage(Keepers, ID) -> 
    io:fwrite("trie garbage \n"),
    gen_server:call({global, ids:main(ID)}, {garbage, Keepers}).
garbage_leaves(KLS, ID) ->
    gen_server:call({global, ids:main(ID)}, {garbage_leaves, KLS}).
integer2path(I, M) -> flip_bytes(<<I:M>>).
flip_bytes(X) -> flip_bytes(X, <<>>).
flip_bytes(<<>>, X) -> X;
flip_bytes(<<N:8, T/bitstring>>, X) -> 
    flip_bytes(T, <<N:8, X/bitstring>>).
to_path(X, ID) ->
    gen_server:call({global, ids:main(ID)}, {to_path, X}).

