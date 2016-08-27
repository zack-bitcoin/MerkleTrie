-module(dump).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1]).
init({WordSize}) -> {ok, {WordSize}}.
start_link(WordSize, Id) -> gen_server:start_link({global, Id}, ?MODULE, {WordSize}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Location, Id}, X) ->
    dump_bits:delete(Id, Location),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({write, Data, ID}, _From, X) ->
    {Word} = X,
    Top = dump_bits:top(ID),
    file_manager:write(ID, Top*Word, Data),
    dump_bits:write(ID),
    {reply, Top, X};
handle_call({read, Location, ID}, _From, X) ->
    {Word} = X,
    Z = case file_manager:read(ID, Location*Word, Word) of
	{ok, A} -> A;
	eof -> <<0:(Word*8)>>
    end,
    {reply, Z, X};
handle_call(word, _From, X) ->
    {Word} = X,
    {reply, Word, X};
handle_call({highest, ID}, _From, X) ->
    {Word} = X,
    H = file_manager:size(ID),
    %H = dump_bits:top(ID),
    {reply, H, X};
handle_call(Y, _From, X) -> 
    io:fwrite("fail\n"),
    io:fwrite(Y),
    {reply, X, X}.

delete(X, ID) -> gen_server:cast({global, ID}, {delete, X, ID}).
put(Data, ID) -> 
    Word = size(Data),
    Word = word(ID),
    gen_server:call({global, ID}, {write, Data, ID}).
get(X, ID) -> gen_server:call({global, ID}, {read, X, ID}).
word(ID) -> gen_server:call({global, ID}, word).
highest(ID) -> gen_server:call({global, ID}, {highest, ID}).
