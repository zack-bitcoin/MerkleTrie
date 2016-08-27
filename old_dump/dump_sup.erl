-module(dump_sup).
-behaviour(supervisor).
-export([start_link/2,init/1,stop/0]).
start_link(ID, Size) -> 
    L = atom_to_list(ID),
    A = list_to_atom(L ++ "sup"),
    supervisor:start_link({global, A}, ?MODULE, [ID, Size]).
stop() -> halt().

init([ID, Size]) ->
    %ID = kv,%temporary for testing
    L = atom_to_list(ID),
    L1 = L++".db",
    L2 = L++"_bits.db",
    A1 = ID,
    A2 = list_to_atom(L++"_bits"),
    A3 = list_to_atom(L++"_file"),
    A4 = list_to_atom(L++"_bits_file"),
    Children = [{A1, {dump, start_link, [Size, A1]}, permanent, 5000, worker, [dump]},
		{A3, {file_manager, start_link, [L1, A3]}, permanent, 5000, worker, [file_manager]},
		{A4, {file_manager, start_link, [L2, A4]}, permanent, 5000, worker, [file_manager]},
		{A2, {dump_bits, start_link, [A2]}, permanent, 5000, worker, [dump_bits]}
	       ], 
    {ok, { {one_for_one, 5, 10}, Children} }.

