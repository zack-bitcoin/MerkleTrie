-module(trie_sup).
-behaviour(supervisor).
-export([start_link/5,init/1,stop/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
start_link(KeyLength, Size, ID, Amount, Mode) -> 
    %keylength is the number of bytes to encode the path that you follow on the trie.
    CFG = cfg:new(KeyLength, Size, ID),
    supervisor:start_link({global, cfg:id(CFG)}, ?MODULE, [CFG, Amount, Mode]).
stop() -> halt().
init([CFG, Amount, Mode]) ->
    %Size is the size of the data we store in the trie.
    KeyLength = cfg:path(CFG),
    Size = cfg:value(CFG),
    ID = cfg:id(CFG),
    IDS = atom_to_list(ID),
    A2 = list_to_atom(IDS++"_bits"),
    A3 = ids:leaf(CFG),
    A4 = ids:stem(CFG),
    A5 = ids:main(CFG),
    L2 = "data/" ++ IDS ++ "_trie_bits.db",
    Children = [{A3, {dump_sup, start_link, [A3, KeyLength+Size, Amount, Mode]}, permanent, 5000, supervisor, [dump_sup]},
		{A4, {dump_sup, start_link, [A4, 196+(16*KeyLength), Amount, Mode]}, permanent, 5000, supervisor, [dump_sup]},
		%{A1, {file_manager, start_link, [L1, A1, Amount div 8, ram]}, permanent, 5000, worker, [file_manager]},
		{A2, {bits, start_link, [A2, L2, Amount]}, permanent, 5000, worker, [bits]},
		{A5, {trie, start_link, [CFG]}, permanent, 5000, worker, [trie]}
	       ],
    {ok, { {one_for_one, 5, 10}, Children} }.
