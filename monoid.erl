-module(monoid).

-record(monoid, { identity :: monoid_element()
                , operation :: associative_binary_operation()
                }).
-type monoid_element() :: term().
-type associative_binary_operation() :: fun((monoid_element(), monoid_element()) -> monoid_element()).
-type t() :: #monoid{}.
-export_type([t/0]).

-export([ new/2 ]).
-export([ identity/1
        , operate/3
        ]).
-export([ reduce/2
        , concurrent_reduce/2
        ]).

-spec new(Identity :: monoid_element(), Operation :: associative_binary_operation()) -> t().
new(I, O) -> #monoid{identity = I, operation = O}.

-spec identity(t()) -> monoid_element().
identity(#monoid{identity = I}) -> I.

-spec operate(t(), monoid_element(), monoid_element()) -> monoid_element().
operate(#monoid{operation = O}, E1, E2) -> O(E1, E2).


-spec reduce(t(), list(monoid_element())) -> monoid_element().
reduce(#monoid{identity = I, operation = O}, Es) -> lists:foldr(O, I, Es).

-spec concurrent_reduce(t(), list(monoid_element())) -> monoid_element().
concurrent_reduce(_, [E1]) -> E1;
concurrent_reduce(#monoid{operation = O}, [E1, E2]) -> O(E1, E2);
concurrent_reduce(#monoid{operation = O} = M, Es) ->
    N = length(Es),
    {LeftArgs, RightArgs} = lists:split(N div 2, Es),
    Collector = self(),
    LeftWorker = spawn(fun() -> Collector ! {self(), concurrent_reduce(M, LeftArgs)} end),
    RightWorker = spawn(fun() -> Collector ! {self(), concurrent_reduce(M, RightArgs)} end),
    ReceiveFrom = fun(From) -> receive {From, Res} -> Res end end,
    LeftResult = ReceiveFrom(LeftWorker),
    RightResult = ReceiveFrom(RightWorker),
    O(LeftResult, RightResult).
