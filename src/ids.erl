-module(ids).
-export([leaf/1, main/1, stem/1, bits/1]).


leaf(ID) -> list_to_atom(atom_to_list(ID) ++ "_leaf").
stem(ID) -> list_to_atom(atom_to_list(ID) ++ "_stem").
main(ID) -> list_to_atom(atom_to_list(ID) ++ "_main").
bits(ID) -> list_to_atom(atom_to_list(ID) ++ "_bits").
