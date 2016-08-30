-module(trie_sup).
-behaviour(supervisor).
-export([start_link/2,init/1,stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
start_link(Size, Max) -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Size, Max]).
stop() -> halt().
init([Size, Max]) ->
    %S is the size of the data we store in the trie.
    %depluralize nouns to stop namespace overlap with dup_sup
    L1 = "trie_bit.db",
    A1 = trie_bit_file,
    A2 = trie_bit,
    Children = [{leaf, {dump_sup, start_link, [leaf, 12+Size]}, permanent, 5000, supervisor, [dump_sup]},
		{stem, {dump_sup, start_link, [stem, 276]}, permanent, 5000, supervisor, [dump_sup]},
		{A1, {file_manager, start_link, [L1, A1]}, permanent, 5000, worker, [file_manager]},
		{A2, {dump_bits, start_link, [A2]}, permanent, 5000, worker, [dump_bits]},
		{trie, {trie, start_link, [Size, Max]}, permanent, 5000, worker, [trie]}],
    {ok, { {one_for_one, 5, 10}, Children} }.
