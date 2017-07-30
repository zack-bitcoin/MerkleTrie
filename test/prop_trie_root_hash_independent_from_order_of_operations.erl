-module(prop_trie_root_hash_independent_from_order_of_operations).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-define(ID, triePropTest).

prop_put_each_key_only_once_and_no_delete() ->
    {ok, Cwd} = file:get_cwd(),
    DataDir = filename:join([Cwd, "data"]),
    trie_test_utils:cleanup_for_clean_data_dir(DataDir),
    ?FORALL({DistinctLeafKeys, ShuffledDistinctLeafKeys},
	    distinct_leaf_keys_and_shuffled(0, 99),
	    begin
		?assertEqual(length(DistinctLeafKeys),
			     length(ShuffledDistinctLeafKeys)),
		RH1 = trie_put_all_and_compute_root_hash_in_clean_data_dir(
			DataDir, ?ID, 0, DistinctLeafKeys),
		RH2 = trie_put_all_and_compute_root_hash_in_clean_data_dir(
			DataDir, ?ID, 0, ShuffledDistinctLeafKeys),
		%% io:format(
		%%   "Put ~p elements in two trees and comparing their root hashes ~w and ~w.~nInitial elements in first tree are:~n  ~w~nInitial elements in second tree are:~n  ~w~n",
		%%   [length(DistinctLeafKeys),
		%%    RH1, RH1,
		%%    lists:sublist(DistinctLeafKeys, 3),
		%%    lists:sublist(ShuffledDistinctLeafKeys, 3)]),
		RH1 == RH2
	    end).

trie_put_all_and_compute_root_hash_in_clean_data_dir(DataDir, Id, Root, Keys) ->
    R1 = trie_test_utils:setup_for_clean_data_dir(DataDir),
    R2 = setup_trie(Id),
    NewRoot =
	lists:foldl(
	  fun(K, R) ->
		  trie:put(K, _V = <<K:(2*8)>>, _M = 0, R, Id)
	  end,
	  Root,
	  Keys),
    RH = trie:root_hash(Id, NewRoot),
    cleanup_trie(R2),
    trie_test_utils:cleanup_for_clean_data_dir(R1),
    RH.

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
    trie_test_utils:cleanup_alive_sup(SupPid).

distinct_leaf_keys_and_shuffled(Low, High) ->
    ?LET(Ks, distinct_leaf_keys(Low, High), {Ks, shuffle(Ks)}).

distinct_leaf_keys(Low, High) ->
    ulist(leaf_key(Low, High)).

leaf_key(Low, High) ->
    integer(Low, High).

%% From https://github.com/manopapad/proper/blob/v1.2/test/proper_tests.erl#L1457-L1458
ulist(ElemType) ->
    ?SUCHTHAT(L, list(ElemType), no_duplicates(L)).
%% From https://github.com/manopapad/proper/blob/v1.2/test/proper_tests.erl#L1219-L1220
no_duplicates(L) ->
    length(lists:usort(L)) =:= length(L).

%% From https://github.com/manopapad/proper/blob/v1.2/test/proper_tests.erl#L1306-L1309
shuffle([]) ->
    [];
shuffle(L) ->
    ?LET(X, elements(L), [X | shuffle(lists:delete(X,L))]).
