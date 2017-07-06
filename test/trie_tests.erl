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
	     cleanup_alive_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Initialization produces empty trie",
	?_test(assert_trie_empty(_Root = 0, ?ID))}
     , {"Put - happy path", fun put_happy_path/0}
     , {"Get - case empty", fun get_empty/0}
     , {"Garbage collection keeping a root (i.e. a stem)", fun gc_keeping_root/0}
     , {"Garbage collection keeping a leaf", fun gc_keeping_leaf/0}
     ]
    }.

assert_trie_empty(Root = 0, Id) ->
    ?assertEqual([], trie:get_all(Root, Id)),
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
