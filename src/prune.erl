-module(prune).
-export([stem/3]).
stem(Old, New, CFG) ->
    Stem1 = stem:get(Old, CFG),
    Stem2 = stem:get(New, CFG),
    dump:delete(Old, ids:stem(CFG)),
    loop(CFG, Stem1, Stem2, 1, []).
leaf(Old, CFG) -> dump:delete(Old, ids:leaf(CFG)).
loop(_, _, _, 17, R) -> R;
loop(CFG, S1, S2, N, R) ->
    %Types-  0:empty, 1:stem, 2:leaf
    T1 = stem:type(N, S1),
    T2 = stem:type(N, S2),
    H1 = element(N, stem:hashes(S1)),
    H2 = element(N, stem:hashes(S2)),
    P1 = stem:pointer(N, S1),
    P2 = stem:pointer(N, S2),
    R2 = if
	(T1 == 0) or (H1 == H2) -> [];%nothing to clean.
	true -> 
	    case {T1, T2} of
		{1, 1} -> stem(P1, P2, CFG);
		{1, _} -> stem(P1, 0, CFG);
		{2, _} -> leaf(P1, CFG), 
			  [{P1, P2}]
	    end
    end,
    loop(CFG, S1, S2, N+1, R ++ R2).
