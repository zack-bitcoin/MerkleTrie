-module(dump).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1,update/3, put_batch/2]).
-record(db, {top, id, loc}).
init({ID, Loc}) -> 
    process_flag(trap_exit, true),
    case ets:info(ID) of
	undefined ->
	    case ets:file2tab(Loc) of
		{ok, ID} -> ok;
		{error, R} ->
                                                %io:fwrite(R),
                                %io:fwrite("make table "),
                                %io:fwrite(ID),
                                %io:fwrite("\n"),
		    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
	    end;
	_ -> ok
    end,
    W = case db:read(loc2rest(Loc)) of
	    "" -> 1;
	    X -> 
		{Y} = binary_to_term(X),
		Y
	end,
    F = #db{top = W, id = ID, loc = Loc},
    {ok, F}.
start_link(Id, Loc) -> 
    X = {Id, Loc},
    gen_server:start_link({global, Id}, ?MODULE, X, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    Loc2 = F ++ "_rest.db".
save_table(ID, Loc) ->
    case ets:tab2file(ID, Loc, [{sync, true}]) of
        ok -> ok;
        {error, R} ->
            %io:fwrite(R),
            %timer:sleep(200),
            save_table(ID, Loc)
    end.
%terminate(_, {Top, ID, Loc}) -> 
terminate(_, DB) -> 
    %Loc2 = loc2rest(Loc),
    Loc2 = loc2rest(DB#db.loc),
    %db:save(Loc2, term_to_binary({Top})),
    db:save(Loc2, term_to_binary({DB#db.top})),
    %save_table(ID, Loc),
    save_table(DB#db.id, DB#db.loc),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, []) -> {noreply, []};
handle_cast(_, X) -> {noreply, X}.
handle_call({write_batch, L}, _, DB) ->
    %ets:insert(ID, L),
    ets:insert(DB#db.id, L),
    %Top2 = max_second(L, Top),
    Top2 = max_second(L, DB#db.top),
    DB2 = DB#db{top = Top2+1},
    {reply, ok, DB2};
handle_call({delete, Location}, _From, DB) ->
    %ets:delete(Id, Location),
    ets:delete(DB#db.id, DB#db.loc),
    {reply, ok, DB};
handle_call({update, Location, Data}, _From, DB) ->
    %ets:insert(ID, [{Location, Data}]),
    ets:insert(DB#db.id, [{Location, Data}]),
    {reply, ok, DB};
handle_call({write, Data}, _From, DB) ->
    %ets:insert(ID, {Top, Data}),
    Top = DB#db.top,
    ets:insert(DB#db.id, {Top, Data}),
    DB2 = DB#db{top = Top+1},
    {reply, Top, DB2};
handle_call({read, Location}, _From, DB) ->
    %Y = case ets:lookup(ID, Location) of
    Y = case ets:lookup(DB#db.id, Location) of
            [] -> empty;
            Z -> element(2, hd(Z))
        end,
    {reply, Y, DB};
handle_call({highest}, _From, DB) ->
    Top = DB#db.top,
    {reply, Top, DB};
handle_call(off, _, X) -> {reply, ok, X};
handle_call(Other, _, X) ->
    io:fwrite("dump cannot handle that command\n"),
    io:fwrite("\n"),
    %io:fwrite({Other, X}),
    {reply, ok, X}.


max_second([], X) -> X;
max_second([{L, D}|T], X) ->
    max_second(T, max(X, L)).


off(ID) -> gen_server:call({global, ID}, off).
delete(X, ID) -> gen_server:call({global, ID}, {delete, X}).
update(Location, Data, ID) -> 
    gen_server:call({global, ID}, {update, Location, Data}).
put(Data, ID) -> 
    gen_server:call({global, ID}, {write, Data}).
put_batch(L, ID) -> 
    gen_server:call({global, ID}, {write_batch, L}).
get(X, ID) -> 
    true = X > 0,
    gen_server:call({global, ID}, {read, X}).
word(ID) -> 0.
highest(ID) -> gen_server:call({global, ID}, {highest}).
