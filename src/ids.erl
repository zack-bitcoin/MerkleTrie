-module(ids).
-export([main_id/1, leaf/1, main/1, stem/1, bits/1, ram/1]).


leaf(CFG) -> list_to_atom(atom_to_list(cfg:id(CFG)) ++ "_leaf").
stem(CFG) -> list_to_atom(atom_to_list(cfg:id(CFG)) ++ "_stem").
main(CFG) -> main_id(cfg:id(CFG)).
bits(CFG) -> list_to_atom(atom_to_list(cfg:id(CFG)) ++ "_bits").
main_id(ID) -> list_to_atom(atom_to_list(ID) ++ "_main").
ram(CFG) -> list_to_atom(atom_to_list(cfg:id(CFG)) ++ "_ram").
