-module(prop_trie_history_readable_at_any_height).

-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([apply_ops_and_gc/4]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-define(DEBUG, false).

-define(INITIAL_EMPTY_ROOT, 0).
-define(ID, triePropTest).
-define(VALUE_SIZE, 2).

prop_from_clean_data_dir() ->
    {ok, Cwd} = file:get_cwd(),
    DataDir = filename:join([Cwd, "data"]),
    trie_test_utils:cleanup_for_clean_data_dir(DataDir),
    numtests(
      25, %% Default is 100.
      ?FORALL(Cmds, commands(?MODULE),
	      begin
		  R1 = trie_test_utils:setup_for_clean_data_dir(DataDir),
		  R2 =
		      setup_trie(
			?ID,
			%% This function test produces and keeps a
			%% large number of leaves and stems.  It
			%% avoids hitting the amount passed at trie
			%% instantiation by choosing a high value.
			_Amount = 1000000),
		  debug_fmt("Going to run commands: ~p~n", [Cmds]),
		  {History, State, Result} = run_commands(?MODULE, Cmds),
		  cleanup_trie(R2),
		  trie_test_utils:cleanup_for_clean_data_dir(R1),
		  ?WHENFAIL(io:format("Commands: ~p~nHistory: ~p~nState: ~p~nResult: ~p~n",
				      [Cmds, History, State, Result]),
			    aggregate(command_names(Cmds), Result =:= ok))
	      end)).

setup_trie(Id, Amount) ->
    {ok, SupPid} = trie_sup:start_link(
		     _KeyLength = 9,
		     _Size = ?VALUE_SIZE,
		     _ID = Id,
		     Amount,
		     _Meta = 2,
		     _HashSize = 12,
		     _Mode = hd),
    SupPid.

cleanup_trie(SupPid) ->
    trie_test_utils:cleanup_alive_sup(SupPid).

-type height() :: non_neg_integer().
-record(state_at_height,
	{root :: stem:stem_p(),
	 kvs :: dict:dict(leaf:key(), height()) %% Record only key and height at which key was written - in order to save memory if configuring large value size.
	}).
-record(state, {chain :: dict:dict(height(),
				   #state_at_height{}),
		height :: height()}).

initial_state() ->
    debug_fmt("In initial_state. (Pid ~p.)~n", [self()]),
    H = 0,
    StateAtHeight0 = #state_at_height{root = ?INITIAL_EMPTY_ROOT,
				      kvs = dict:new()},
    #state{chain = dict:from_list([{H, StateAtHeight0}]),
	   height = H}.

command(State) ->
    CurrentHeight = State#state.height,
    CurrentRoot = (dict:fetch(CurrentHeight, State#state.chain)
		  )#state_at_height.root,
    PreviousRoots =
	lists:map(
	  fun({_H, SaH}) -> SaH#state_at_height.root end,
	  dict:to_list(
	    dict:filter(
	      fun(H, _) -> H =/= CurrentHeight end,
	      State#state.chain))),
    oneof([{call,
	    ?MODULE, apply_ops_and_gc,
	    [list({oneof([put, delete]), leaf_key()}),
	     CurrentHeight, CurrentRoot,
	     PreviousRoots
	    ]}
	  ]).

precondition(#state{}, {call, _Mod, _Fun, _Args}) ->
    true.

leaf_key() ->
    integer(1, 100).

apply_ops_and_gc(Ops, CurrentHeight, CurrentRoot, PreviousRoots) ->
    NewHeight = CurrentHeight + 1,
    NewRoot =
	lists:foldl(
	  fun
	      ({put, K}, R) ->
		  V = leaf_value_from_key_and_height(K, NewHeight),
		  debug_fmt("(Current height ~p, new height ~p) Going to put leaf key ~p value ~p.~n", [CurrentHeight, NewHeight, K, V]),
		  trie:put(K, V, _M = 0, R, ?ID);
	      ({delete, K}, R) ->
		  debug_fmt("(Current height ~p, new height ~p) Going to delete leaf key ~p.~n", [CurrentHeight, NewHeight, K]),
		  trie:delete(K, R, ?ID)
	  end,
	  CurrentRoot,
	  Ops),
    ok = trie:garbage([NewRoot, CurrentRoot | PreviousRoots], ?ID),
    NewRoot.

leaf_value_from_key_and_height(K, H) ->
    <<(K+H):(?VALUE_SIZE*8)>>.

postcondition(State,
	      {call, ?MODULE, apply_ops_and_gc, Args = [_, H, _, _]},
	      _Res = NewRoot) when H =:= State#state.height ->
    debug_fmt("(Height ~p) In postcondition. (Pid ~p.)~n", [State#state.height, self()]),
    [Ops, _, _, _] = Args,
    NewState = next_state_internal(State, Ops, NewRoot),
    is_history_readable_at_any_height(NewState).

next_state(State,
	   _Res = NewRoot,
	   {call, ?MODULE, apply_ops_and_gc, Args}) ->
    debug_fmt("(Height ~p) In next_state. (Pid ~p.)~n", [State#state.height, self()]),
    [Ops, _, _, _] = Args,
    next_state_internal(State, Ops, NewRoot).

next_state_internal(State, Ops, NewRoot) ->
    #state{chain = Chain, height = Height} = State,
    #state_at_height{kvs = KVs} = dict:fetch(Height, Chain),
    NewHeight = Height + 1,
    NewKVs =
	lists:foldl(
	  fun
	      ({put, K}, AccKVs) ->
		  dict:store(K, NewHeight, AccKVs);
	      ({delete, K}, AccKVs) ->
		  dict:erase(K, AccKVs)
	  end,
	  KVs,
	  Ops),
    ?assertEqual(error, dict:find(NewHeight, Chain)),
    NewChain = dict:store(NewHeight,
			  #state_at_height{root = NewRoot,
					   kvs = NewKVs},
			  Chain),
    State#state{chain = NewChain,
		height = NewHeight}.

is_history_readable_at_any_height(State) ->
    lists:all(
      fun({H, SaH}) ->
	      are_datum_and_proof_of_each_key_readable_and_correct(
		_TopHeight = State#state.height,
		H, SaH)
      end,
      dict:to_list(State#state.chain)).

are_datum_and_proof_of_each_key_readable_and_correct(TopHeight, Height, SaH) ->
    #state_at_height{root = Root, kvs = KVs} = SaH,
    lists:all(
      fun({K, HeightAtWhichKeyWasPut}) ->
	      are_datum_and_proof_of_key_readable_and_correct(
		TopHeight,
		Height,
		K, HeightAtWhichKeyWasPut,
		Root)
      end,
      dict:to_list(KVs)).

are_datum_and_proof_of_key_readable_and_correct(
  TopHeight,
  Height,
  Key, HeightAtWhichKeyWasPut,
  Root) ->
    Value = leaf_value_from_key_and_height(Key, HeightAtWhichKeyWasPut),
    {RootHash, Leaf, Proof} = trie:get(Key, Root, ?ID),
    DatumKey = leaf:key(Leaf),
    IsDatumKeyOk = (DatumKey =:= Key),
    case IsDatumKeyOk of
	true -> ok;
	false -> fmt("(Top height ~p, height ~p) Bad datum key: expected ~p, actual ~p.~n", [TopHeight, Height, Key, DatumKey])
    end,
    DatumValue = leaf:value(Leaf),
    IsDatumValueOk = (DatumValue =:= Value),
    case IsDatumValueOk of
	true -> ok;
	false -> fmt("(Top height ~p, height ~p) Bad datum value for key ~p: expected ~p, actual ~p.~n", [TopHeight, Height, Key, Value, DatumValue])
    end,
    IsProofOk = verify:proof(RootHash, Leaf, Proof, trie:cfg(?ID)),
    case IsProofOk of
	true -> ok;
	false -> fmt("(Top height ~p, height ~p) Bad proof for key ~p: root hash ~p, leaf ~p, proof ~p.~n", [TopHeight, Height, Key, RootHash, Leaf, Proof])
    end,
    IsDatumKeyOk and IsDatumValueOk and IsProofOk.

debug_fmt(Fmt, Data) when ?DEBUG =:= true ->
    fmt(Fmt, Data);
debug_fmt(_, _) when ?DEBUG =:= false ->
    ok.

fmt(Fmt, Data) ->
    io:format(Fmt, Data).
