-module(db_sup).
-behaviour(supervisor).
-export([start_link/2,init/1,stop/0]).
start_link(ID, Size) -> supervisor:start_link({global, ID}, ?MODULE, [ID, Size]).
stop() -> halt().

init([ID, Size]) ->
    L = atom_to_list(ID),
    L1 = L++".db",
    L2 = L++"_bits.db",
    A1 = list_to_atom(L++"_db"),
    A2 = list_to_atom(L++"_bits"),
    Children = [{A1, {fixed_size_dump, start_link, [L1, Size, A1]}, permanent, 5000, worker, [fixed_size_dump]},
		{A2, {dump_bits, start_link, [L2, A2]}, permanent, 5000, worker, [dump_bits]}
	       ], 
    {ok, { {one_for_one, 5, 10}, Children} }.

