-module(test_dump).
-export([test/0]).
test() ->
    ID = kv,
    dump_sup:start_link(ID, 5),
    V1 = <<5,4,3,2,1>>,
    V2 = <<5,4,3,2,1>>,
    V3 = <<5,4,3,2,1>>,
    A1 = dump:put(V1, ID),
    V1 = dump:get(A1, ID),
    A2 = dump:put(V2, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    A3 = dump:put(V3, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    V3 = dump:get(A3, ID),
    dump:delete(A2, ID),
    A2 = dump:put(V1, ID),
    test_times(10000, ID),
    dump:highest(ID).
test_times(0, _) -> success;
test_times(N, ID) -> 
    dump:put(<<0,0,0,0,0>>, ID),
    test_times(N-1, ID).
