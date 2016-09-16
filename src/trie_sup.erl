-module(trie_sup).
-behaviour(supervisor).
-export([start_link/4,init/1,stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
start_link(Size, ID, WS, KeyLength) -> supervisor:start_link({global, ID}, ?MODULE, [Size, ID, WS, KeyLength]).%WS is the number of bytes to store the amount of money controlled by each branch of the trie.
stop() -> halt().
init([Size, ID, WS, KeyLength]) ->
    %Size is the size of the data we store in the trie.
    IDS = atom_to_list(ID),
    L1 = "data/"++IDS++"_trie.db",
    A1 = list_to_atom(IDS++"_bits_file"),
    A2 = list_to_atom(IDS++"_bits"),
    A3 = ids:leaf(ID),
    A4 = ids:stem(ID),
    A5 = ids:main(ID),
    L2 = "data/" ++ IDS ++ "_trie_bits.db",
    LS = WS + Size + KeyLength,
    Children = [{A3, {dump_sup, start_link, [A3, WS+KeyLength+Size]}, permanent, 5000, supervisor, [dump_sup]},
		{A4, {dump_sup, start_link, [A4, 276+(16*WS)]}, permanent, 5000, supervisor, [dump_sup]},
		{A1, {file_manager, start_link, [L1, A1]}, permanent, 5000, worker, [file_manager]},
		{A2, {dump_bits, start_link, [A2, L2]}, permanent, 5000, worker, [dump_bits]},
		{A5, {trie, start_link, [Size, KeyLength, ID, WS]}, permanent, 5000, worker, [trie]}
	       ],
    {ok, { {one_for_one, 5, 10}, Children} }.
