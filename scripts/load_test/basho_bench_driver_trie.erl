-module(basho_bench_driver_trie).

-export([new/1,
         run/4]).

%% Macros compatible with header `basho_bench.hrl` - in order not to
%% require including it for compiling this module:
-define(DEBUG(Str, Args), error_logger:info_msg(Str, Args)).
-define(INFO(Str, Args), error_logger:info_msg(Str, Args)).
-define(WARN(Str, Args), error_logger:warning_msg(Str, Args)).
-define(ERROR(Str, Args), error_logger:error_msg(Str, Args)).

-record(state, {
          trie_id,
          root
         }).

new({_,_,Id}) ->
    TrieId = basho_bench_config:get(trie_id),
    KeySizeBytes = basho_bench_config:get(trie_key_size_bytes),
    ValueSizeBytes = basho_bench_config:get(trie_value_size_bytes),
    MetaSizeBytes = 0 = basho_bench_config:get(trie_meta_size_bytes),
    HashSizeBytes = basho_bench_config:get(trie_hash_size_bytes),
    Amount = basho_bench_config:get(trie_amount),
    Mode = basho_bench_config:get(trie_mode),
    {ok, TrieSupPid} =
        trie_sup:start_link( %% TODO review linking
          KeySizeBytes,
          ValueSizeBytes,
          TrieId,
          Amount,
          MetaSizeBytes,
          HashSizeBytes,
          Mode),
    ?INFO("Worker ~p (~p) using trie with id ~p and sup ~p.\n", [Id, self(), TrieId, TrieSupPid]),
    Root = basho_bench_config:get(trie_initial_empty_root),
    {[], _} = {trie:get_all(Root, TrieId), {{worker_id, Id}, {trie_initial_allegedly_empy_root, Root}}},
    State = #state{trie_id = TrieId, root = Root},
    {ok, State}.

run(get, KeyGen, _ValueGen, State = #state{}) ->
    Key = KeyGen(),
    case trie:get(Key, State#state.root, State#state.trie_id) of
        {_, empty, _} ->
            {ok, State};
        {_, _, _} ->
            {ok, State};
        Unknown ->
            {error, Unknown}
    end.
