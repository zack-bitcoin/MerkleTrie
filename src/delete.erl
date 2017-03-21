-module(delete).
-export([delete/3]).

delete(ID, Root, CFG) ->
    Path = leaf:path_maker(ID, CFG),
    Branch = store:get_branch(Path, 0, Root, [], CFG),
    %{_, Leaf, Proof} = get:get(Path, Root, CFG),
    X = cfg:hash_size(CFG)*8,
    %X = hash:hash_depth()*8,
    EmptyHash = <<0:X>>,
    {_, NewRoot, _} = store:store_branch(Branch, Path, 0, 0, EmptyHash, CFG),
    NewRoot.

