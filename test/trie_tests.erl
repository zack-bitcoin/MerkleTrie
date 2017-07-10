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
	     cleanup_alive_trie_sup(SupPid),
	     ?assertNot(is_process_alive(SupPid)),
	     ok
     end,
     [ {"Initialization produces empty trie", fun init_as_empty/0}
     , {"Put - happy path", fun put_happy_path/0}
     , {"Get - case empty", fun get_empty/0}
     , {"Put long key", fun put_long_key/0}
     , {"Garbage collection keeping a root (i.e. a stem)", fun gc_keeping_root/0}
     ]
    }.

init_as_empty() ->
    assert_trie_empty(_Root = 0, ?ID).

assert_trie_empty(Root = 0, Id) ->
    ?assertEqual([], trie:get_all(Root, Id)),
    Cfg = trie:cfg(Id),
    ?assertEqual(stem:new_empty(Cfg), stem:get(Root, Cfg)),
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

put_long_key() ->
    Root = 0,
    assert_trie_empty(Root, ?ID),
    PathLengthBytes = cfg:path(trie:cfg(?ID)),
    Key = 1 bsl (PathLengthBytes * 8),
    V = <<1,1>>,
    Meta = 0,
    Root2 = trie:put(Key, V, Meta, Root, ?ID),
    ?assertMatch({_, empty, _}, trie:get(Key, Root2, ?ID)),
    <<KeyUsedForPut:PathLengthBytes>> = <<Key:PathLengthBytes>>,
    ?assertNotEqual(Key, KeyUsedForPut),
    ?debugFmt("~nKey specified: ~p  Key actually used: ~p~n", [Key, KeyUsedForPut]),
    {_, Leaf, _} = trie:get(KeyUsedForPut, Root2, ?ID),
    ?assertEqual(KeyUsedForPut, leaf:key(Leaf)),
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

cleanup_alive_trie_sup(Sup) when is_pid(Sup) ->
    SupMonRef = erlang:monitor(process, Sup),
    unlink(Sup),
    exit(Sup, Reason = shutdown),
    receive
	{'DOWN', SupMonRef, process, Sup, R} ->
	    Reason = R,
	    ok
    end,
    ok.
