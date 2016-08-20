-module(dump_bits).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/1,delete/2]).
init(File) -> {ok, {top_internal(File), File}}.
start_link(File, Id) -> gen_server:start_link({local, Id}, ?MODULE, File, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Height}, {Top, File}) -> 
    %do not call this on an empty bit.
    {ok, B} = file:open(File, [write, read, binary, raw]),
    flip_bit(File, B, Height),
    file:close(B),
    {noreply, {min(Top,Height), File}};
handle_cast(write, {Top, File}) -> 
    {ok, B} = file:open(File, [write, read, binary, raw]),
    flip_bit(File, B, Top),
    NewTop = top2(File, B, 0),
    file:close(B),
    {noreply, {NewTop, File}};
handle_cast(_, X) -> {noreply, X}.
handle_call(top, _From, {Top, File}) -> 
    {reply, Top, {Top, File}};
handle_call(_, _From, X) -> {reply, X, X}.
append(File, F, Data) ->
    N = filelib:file_size(File),
    file:pwrite(F, N, Data).
delete(Id, Height) -> 
    gen_server:cast(Id, {delete, Height}).
write(Id) -> gen_server:cast(Id, write).
top(Id) -> gen_server:call(Id, top).
top_internal(File) ->
    case file:open(File, [read, write, binary, raw]) of
	{ok, F} ->
	    T = top2(File, F,0),
	    file:close(F),
	    T;
	{error, _Reason} ->
	    top_internal(File)
    end.
top2(File, F, N) ->
    case file:pread(F, N, 1000) of
	eof ->
	    append(File, F, <<0:8000>>),
	    top2(File, F, N);
	{ok, X} -> 
	    L = top3(X, 0),
	    case L of
		error -> top2(File, F, N+1);
		K -> (N*8000) + K
	    end
    end.
top3(<<>>, _) -> error;
top3(<<0:1, _/bitstring>>, N) -> N;
top3(<<4294967295:32, T/bitstring>>, N) -> top3(T, N+32);
top3(<<65535:16, T/bitstring>>, N) -> top3(T, N+16);
top3(<<255:8, T/bitstring>>, N) -> top3(T, N+8);
top3(<<15:4, T/bitstring>>, N) -> top3(T, N+4);
top3(<<3:2, T/bitstring>>, N) -> top3(T, N+2);
top3(<<1:1, T/bitstring>>, N) -> top3(T, N+1).
flip_bit(File, F, Number) ->
    ND8 = Number div 8,
    {ok, Byte} = case file:pread(F, ND8, 1) of
	eof -> append(File, F, <<0:8000>>),
	       file:pread(F, ND8, 1);
	{ok, ABC} -> {ok, ABC}
    end,
    <<Num:8>> = Byte,
    NewNum = Num bxor round(math:pow(2, (7 - (Number rem 8)))),
    NewByte = <<NewNum:8>>,
    file:pwrite(F, ND8, NewByte).
    
test() ->
    ok.
