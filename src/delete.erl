-module(delete).
-export([delete/3]).

-spec delete(leaf:key(), stem:stem_p(), cfg:cfg()) -> stem:stem_p().
delete(ID, Root, CFG) ->
    Path = leaf:path_maker(ID, CFG),
    case store:get_branch(Path, 0, Root, [], CFG) of
	{_, _, _} -> %already no leaf with the specified path
	    Root;
	Branch ->
	    X = cfg:hash_size(CFG)*8,
	    EmptyHash = <<0:X>>,
	    {_, NewRoot, _} = store:store_branch(Branch, Path, 0, 0, EmptyHash, CFG),
	    NewRoot
    end.

