-module(test_tuple).
-export([test/0]).
%The result of this experiment is that setelement is very innefficient. It copies the tuples recursively.
test() ->
    X = list_to_tuple(make_list(0,10000000)),
    Y = {0, X}.
    %Z = setelement(1, Y, 1).
make_list(_, 0) -> [];
make_list(X, N) -> [X|make_list(X, N-1)].
