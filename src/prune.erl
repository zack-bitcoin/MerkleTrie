-module(prune).
-export([stem/3, garbage/3]).
%stem/3 is for pruning an old part of our history we no longer care about.
%Each batch needs to be pruned in order to completely clear the memory.
stem(Old, New, CFG) ->
    Stem1 = stem:get(Old, CFG),
    Stem2 = stem:get(New, CFG),
    dump:delete(Old, ids:stem(CFG)),
    loop(CFG, Stem1, Stem2, 1, [], fun(A, B, C) -> stem(A, B, C) end).

%garbage/3 is for undoing a batch, to recover the space.
%For example, if you are mining on a blockchain, and the block you are mining on does not get included. You want to recover the memory that was used by this block.
garbage(Trash, Keep, CFG) ->
    TStem = stem:get(Trash, CFG),
    KStem = stem:get(Keep, CFG),
    if
	Trash == Keep -> ok;
	true -> dump:delete(Trash, ids:stem(CFG))
    end,
    loop(CFG, TStem, KStem, 1, [], fun(A, B, C) -> garbage(A, B, C) end).
loop(_, _, _, 17, R, _) -> R;
loop(CFG, S1, S2, N, R, Stem) ->
    T1 = stem:type(N, S1),
    T2 = stem:type(N, S2),
    H1 = element(N, stem:hashes(S1)),
    H2 = element(N, stem:hashes(S2)),
    P1 = stem:pointer(N, S1),
    P2 = stem:pointer(N, S2),
    R2 = if
	     (T1 == 0) or (H1 == H2) -> [];%nothing to clean
	      true ->
		 case {T1, T2} of%{trash, keep}
		     {1, 1} -> Stem(P1, P2, CFG);
		     {1, _} -> Stem(P1, 0, CFG);
		     {2, _} -> dump:delete(P1, ids:leaf(CFG)), [{P1, P2}]
		 end
	 end,
    loop(CFG, S1, S2, N+1, R ++ R2, Stem).
