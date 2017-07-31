-module(trie_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ID, trieUnitTest).

api_smoke_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     trie_test_utils:cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Initialization produces empty trie",
	?_test(assert_trie_empty(_Root = 0, ?ID))}
     , {"Put - happy path", fun put_happy_path/0}
     , {"Get - case empty", fun get_empty/0}
     , {"Delete - happy path", fun delete_happy_path/0}
     , {"Garbage collection keeping a root (i.e. a stem)", fun gc_keeping_root/0}
     , {"Garbage collection keeping a leaf", fun gc_keeping_leaf/0}
     ]
    }.

root_hash_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     assert_trie_empty(0, ?ID),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     trie_test_utils:cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Hash of root of empty tree",
	fun() ->
		Root = 0,
		RH = trie:root_hash(?ID, Root),
		?assertMatch({RH, empty, _}, trie:get(_Key = 1, Root, ?ID)),
		ok
	end}
     , {"Hash of tree root changes with tree content",
	fun() ->
		Root = 0,
		RH = trie:root_hash(?ID, Root),
		Key = 1,
		Root2 = trie:put(Key, _V = <<1,1>>, _Meta = 0, Root, ?ID),
		RH2 = trie:root_hash(?ID, Root2),
		?assertMatch({RH2, _, _}, trie:get(Key, Root2, ?ID)),
		?assertNotEqual(RH, RH2),
		ok
	end}
     , {"Hash of tree root changes with leaf key",
	fun() ->
		Root = 0,
		K1 = 1,
		K2 = 2,
		V = <<1,1>>,
		Meta = 0,
		Root2A = trie:put(K1, V, Meta, Root, ?ID),
		Root2B = trie:put(K2, V, Meta, Root, ?ID),
		?assertNotEqual(trie:root_hash(?ID, Root2A),
				trie:root_hash(?ID, Root2B)),
		ok
	end}
     , {"Hash of tree root changes with leaf value",
	fun() ->
		Root = 0,
		K = 1,
		V1 = <<1,1>>,
		V2 = <<1,2>>,
		Meta = 0,
		Root2A = trie:put(K, V1, Meta, Root, ?ID),
		Root2B = trie:put(K, V2, Meta, Root, ?ID),
		?assertNotEqual(trie:root_hash(?ID, Root2A),
				trie:root_hash(?ID, Root2B)),
		ok
	end}
     , {"Hash of tree root does not change with leaf meta",
	fun() ->
		Root = 0,
		K = 1,
		V = <<1,1>>,
		Meta1 = 0,
		Meta2 = 1,
		Root2A = trie:put(K, V, Meta1, Root, ?ID),
		Root2B = trie:put(K, V, Meta2, Root, ?ID),
		?assertEqual(trie:root_hash(?ID, Root2A),
			     trie:root_hash(?ID, Root2B)),
		ok
	end}
     , {"Hash of tree root depends on tree content rather than order of operations - case empty tree",
	fun() ->
		Root = 0,
		Key = 1,
		Root2 = trie:put(Key, _V = <<1,1>>, _Meta = 0, Root, ?ID),
		Root3 = trie:delete(Key, Root2, ?ID),
		?assertEqual(trie:root_hash(?ID, Root),
			     trie:root_hash(?ID, Root3)),
		ok
	end}
     , {"Hash of tree root depends on tree content rather than order of operations - case non-empty tree",
	fun() ->
		Root = 0,
		K1 = 1,
		K2 = 2,
		Meta = 0,
		Root2 = trie:put(K1, _V1 = <<1,1>>, Meta, Root, ?ID),
		Root3 = trie:put(K2, _V2 = <<1,2>>, Meta, Root2, ?ID),
		Root4 = trie:delete(K2, Root3, ?ID),
		?assertEqual(trie:root_hash(?ID, Root2),
			     trie:root_hash(?ID, Root4)),
		ok
	end}
     ]
    }.

key_range_good_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     assert_trie_empty(0, ?ID),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     trie_test_utils:cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Put key range - case min key",
	?_test(put_and_assert_key(_Key = 0, _Root = 0, ?ID))}
     , {"Put key range - case max key",
	?_test(put_and_assert_key(
		 _Key = (1 bsl (cfg:path(trie:cfg(?ID)) * 8)) - 1,
		 _Root = 0,
		 ?ID))}
     ]
    }.

key_range_bad_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     SupMonRef = erlang:monitor(process, SupPid),
	     unlink(SupPid),
	     assert_trie_empty(0, ?ID),
	     {SupPid, SupMonRef}
     end,
     fun({SupPid, SupMonRef}) ->
	     receive
		 {'DOWN', SupMonRef, process, SupPid, Reason} ->
		     ?debugFmt("~nTrie sup ~p exited for reason ~p~n",
			       [SupPid, Reason]),
		     ok
	     end,
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Put key range - case negative key",
	?_assertException(
	   _, _,
	   trie:put(_Key = -1, _Value = <<1,1>>, _Meta = 0, _Root = 0, ?ID))}
     , {"Put key range - case too big key",
	?_assertException(
	   _, _,
	   trie:put(_Key = 1 bsl (cfg:path(trie:cfg(?ID)) * 8),
		    _Value = <<1,1>>, _Meta = 0, _Root = 0, ?ID))}
     ]
    }.

delete_unexistent_key_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     assert_trie_empty(0, ?ID),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     trie_test_utils:cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Delete from empty tree",
	?_test(trie:delete(_Key = 0, _Root = 0, ?ID))}
     , {"Delete unexistent key from non-empty tree",
	fun() ->
		%% Test case identified by property-based testing.
		K1 = 90,
		K2 = 26,
		%% The keys share the first nibble of the path.
		?assertEqual(hd(leaf:path_maker(K1, trie:cfg(?ID))),
			     hd(leaf:path_maker(K2, trie:cfg(?ID)))),
		trie:delete(
		  K2,
		  trie:put(K1, _V = <<1,1>>, _Meta = 1, _Root = 0, ?ID),
		  ?ID),
		ok
	end}
     ]
    }.

gc_test_() ->
    {foreach,
     fun() ->
	     ?debugFmt("~nCurrent working directory: ~p~n",
		       [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
	     {ok, SupPid} =
		 trie_sup:start_link(
		   _KeyLength = 9,
		   _Size = 2,
		   _ID = ?ID,
		   _Amount = 1000000,
		   _Meta = 2,
		   _HashSize = 12,
		   _Mode = hd),
	     ?debugFmt("~nTrie sup pid: ~p~n", [SupPid]),
	     assert_trie_empty(0, ?ID),
	     SupPid
     end,
     fun(SupPid) ->
	     ?assert(is_process_alive(SupPid)),
	     trie_test_utils:cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Garbage collection keeping roots (i.e. stems) deletes nothing if specified stem is root",
	fun() ->
		Root = 0,
		Key = 1,
		V = <<1,1>>,
		Meta = 0,
		Root2 = trie:put(Key, V, Meta, Root, ?ID),
		[Leaf] = trie:get_all(Root2, ?ID),
		?assertEqual(ok, trie:garbage([Root2], ?ID)),
		?assertEqual([Leaf], trie:get_all(Root2, ?ID)),
		ok
	end}
     , {"Garbage collection keeping roots (i.e. stems) deletes things that are not descendent of specified stems",
	fun() ->
		Root = 0,
		K1 = 1,
		V1 = <<1,1>>,
		K2 = 2,
		V2 = <<1,2>>,
		Meta = 0,
		Root2A = trie:put(K1, V1, Meta, Root, ?ID),
		Root2B = trie:put(K2, V2, Meta, Root, ?ID),
		{_, L2, _} = trie:get(K2, Root2B, ?ID),
		?assertEqual(V2, leaf:value(L2)),
		{_, L1, _} = trie:get(K1, Root2A, ?ID),
		?assertEqual(V1, leaf:value(L1)),
		?assertEqual(ok, trie:garbage([Root2B], ?ID)),
		?assertMatch({_, empty, _}, trie:get(K1, Root2A, ?ID)),
		?assertEqual([], trie:get_all(Root2A, ?ID)),
		?assertMatch({_, L2, _}, trie:get(K2, Root2B, ?ID)),
		?assertMatch([_], trie:get_all(Root2B, ?ID)),
		ok
	end}
     ]
    }.

assert_trie_empty(Root = 0, Id) ->
    ?assertEqual([], trie:get_all(Root, Id)),
    ok.

put_and_assert_key(Key, Root, Id) ->
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, Id),
    {_, Leaf, _} = trie:get(Key, Root2, Id),
    ?assertEqual(Key, leaf:key(Leaf)),
    ok.

put_happy_path() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Key = 1,
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    {Root2Hash, Leaf, Proof} = trie:get(Key, Root2, ?ID),
    ?assertEqual(V, leaf:value(Leaf)),
    ?assert(verify:proof(Root2Hash, Leaf, Proof, trie:cfg(?ID))),
    ?assertEqual([Leaf], trie:get_all(Root2, ?ID)),
    ok.

get_empty() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    ?assertMatch({_, empty, _}, trie:get(_Key = 1, Root, ?ID)),
    ok.

delete_happy_path() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Key = 1,
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    {_, Leaf, _} = trie:get(Key, Root2, ?ID),
    ?assertEqual(V, leaf:value(Leaf)),
    Root3 = trie:delete(Key, Root2, ?ID),
    ?assertMatch({_, empty, _}, trie:get(Key, Root3, ?ID)),
    ?assertEqual([], trie:get_all(Root3, ?ID)),
    ok.

gc_keeping_root() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Key = 1,
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    ?assertEqual(ok, trie:garbage([Root2], ?ID)),
    {Root2Hash, Leaf, Proof} = trie:get(Key, Root2, ?ID),
    ?assert(verify:proof(Root2Hash, Leaf, Proof, trie:cfg(?ID))),
    ok.

gc_keeping_leaf() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    Cfg = trie:cfg(?ID),
    K1 = 1,
    V1 = <<1,1>>,
    K2 = 2,
    ?assert(K2 > K1),
    V2 = <<1,2>>,
    Meta = 0,
    Root2 = trie:put(K1, V1, Meta, Root, ?ID),
    Root3 = trie:put(K2, V2, Meta, Root2, ?ID),
    {_, Leaf2, _} = trie:get(K2, Root3, ?ID),
    ?assertEqual(ok, trie:garbage_leaves([{leaf:path(Leaf2, Cfg), Root3}], ?ID)),
    {RootHash, Leaf2, Proof} = trie:get(K2, Root3, ?ID),
    ?assert(verify:proof(RootHash, Leaf2, Proof, Cfg)),
    ok.
