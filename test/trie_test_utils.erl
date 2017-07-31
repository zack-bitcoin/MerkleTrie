-module(trie_test_utils).

-export([cleanup_alive_sup/1,
	 setup_for_clean_data_dir/1, cleanup_for_clean_data_dir/1]).

-include_lib("stdlib/include/assert.hrl").

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

setup_for_clean_data_dir(DataDir) ->
    ?assert(filelib:is_dir(DataDir)),
    ?assertEqual({ok, []}, file:list_dir_all(DataDir)),
    DataDir.

cleanup_for_clean_data_dir(DataDir) ->
    DbFiles = filelib:wildcard(filename:join([DataDir, "*.db"])),
    lists:foreach(fun(F) -> {ok, _} = {file:delete(F), F} end, DbFiles),
    ?assertEqual({ok, []}, file:list_dir_all(DataDir)),
    ok.
