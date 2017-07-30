-module(prop_trie_arbitrary_put_and_delete).
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-define(ID, triePropTest).

prop_from_data_dir_as_it_is() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		R = setup_trie(?ID),
		{History, State, Result} = run_commands(?MODULE, Cmds),
		cleanup_trie(R),
		?WHENFAIL(io:format("History: ~p~nState: ~p~nResult: ~p~n",
				    [History, State, Result]),
			  aggregate(command_names(Cmds), Result =:= ok))
	    end).

prop_from_clean_data_dir() ->
    {ok, Cwd} = file:get_cwd(),
    DataDir = filename:join([Cwd, "data"]),
    cleanup_for_clean_data_dir(DataDir),
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		R1 = setup_for_clean_data_dir(DataDir),
		R2 = setup_trie(?ID),
		{History, State, Result} = run_commands(?MODULE, Cmds),
		cleanup_trie(R2),
		cleanup_for_clean_data_dir(R1),
		?WHENFAIL(io:format("History: ~p~nState: ~p~nResult: ~p~n",
				    [History, State, Result]),
			  aggregate(command_names(Cmds), Result =:= ok))
	    end).

setup_trie(Id) ->
    {ok, SupPid} = trie_sup:start_link(
		     _KeyLength = 9,
		     _Size = 2,
		     _ID = Id,
		     _Amount = 1000000,
		     _Meta = 2,
		     _HashSize = 12,
		     _Mode = hd),
    SupPid.

cleanup_trie(SupPid) ->
    cleanup_alive_sup(SupPid).

setup_for_clean_data_dir(DataDir) ->
    ?assert(filelib:is_dir(DataDir)),
    ?assertEqual({ok, []}, file:list_dir_all(DataDir)),
    DataDir.

cleanup_for_clean_data_dir(DataDir) ->
    DbFiles = filelib:wildcard(filename:join([DataDir, "*.db"])),
    lists:foreach(fun(F) -> {ok, _} = {file:delete(F), F} end, DbFiles),
    ?assertEqual({ok, []}, file:list_dir_all(DataDir)),
    ok.

cleanup_alive_sup(Sup) when is_pid(Sup) ->
    SupMonRef = erlang:monitor(process, Sup),
    unlink(Sup),
    exit(Sup, Reason = shutdown),
    receive
	{'DOWN', SupMonRef, process, Sup, R} ->
	    Reason = R,
	    ok
    end,
    ok.

-record(state, {root}).

initial_state() ->
    #state{root = 0}.

command(State) ->
    Root = State#state.root,
    oneof([{call, trie, put, [leaf_key(), <<1,1>>, 0, Root, ?ID]},
	   {call, trie, delete, [leaf_key(), Root, ?ID]}
	  ]).

precondition(#state{}, {call, _Mod, _Fun, _Args}) ->
    true.

leaf_key() ->
    integer(1, 100).

next_state(State, Res, {call, trie, put, _Args}) ->
    State#state{root = Res};
next_state(State, Res, {call, trie, delete, _Args}) ->
    State#state{root = Res}.

postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    true.
