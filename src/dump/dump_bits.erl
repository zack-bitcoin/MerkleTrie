-module(dump_bits).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, test/0,write/1,top/1,delete/2]).
init({ID}) -> {ok, {top_internal(ID), ID}}.
start_link(Id) -> gen_server:start_link({local, Id}, ?MODULE, {Id}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Height}, {Top, ID}) -> 
    flip_bit(ID, Height),
    {noreply, {min(Top,Height), ID}};
handle_cast({write}, {Top, ID}) -> 
    flip_bit(ID, Top),
    NewTop = top2(ID, 0),
    {noreply, {NewTop, ID}};
handle_cast(_, X) -> {noreply, X}.
handle_call(top, _From, {Top, File}) -> 
    {reply, Top, {Top, File}};
handle_call(_, _From, X) -> {reply, X, X}.
append(ID, Data) ->
    N = file_manager:size(ID),
    file_manager:write(ID, N, Data).
delete(ID, Height) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:cast(A2, {delete, Height}).
write(ID) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:cast(A2, {write}).
top(ID) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:call(A2, top).
top_internal(ID) -> top2(ID, 0).
top2(ID, N) ->
    case file_manager:read(ID, N, 1000) of
	eof ->
	    io:fwrite("top2 append\n"),
	    append(ID, <<0:80000>>),
	    top2(ID, N);
	{ok, X} -> 
	    case top3(X, 0) of
		error -> top2(ID, N+1);
		K -> 
		    (N*8000) + K
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
flip_bit(ID, Number) ->
    ND8 = Number div 8,
    Byte = case file_manager:read(ID, ND8, 1) of
	eof ->
	    file_manager:grow(ID),
	    flip_bit(ID, Number);
	{ok, ABC} -> ABC
    end,
    <<Num:8>> = Byte,
    NewNum = Num bxor round(math:pow(2, (7 - (Number rem 8)))),
    NewByte = <<NewNum:8>>,
    file_manager:write(ID, ND8, NewByte).
test() ->
    ok.
