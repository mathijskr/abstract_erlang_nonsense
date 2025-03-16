-module(commutative_monoid).

-record(commutative_monoid, { identity :: commutative_monoid_element()
                            , operation :: commutative_associative_binary_operation()
                            }).
-type commutative_monoid_element() :: term().
-type commutative_associative_binary_operation() :: fun((commutative_monoid_element(), commutative_monoid_element()) -> commutative_monoid_element()).
-type t() :: #commutative_monoid{}.
-export_type([t/0]).

-export([ new/2 ]).
-export([ identity/1
        , operate/3
        ]).
-export([ reduce/2
        , concurrent_reduce/2
        , to_monoid/1
        ]).

-spec new(Identity :: commutative_monoid_element(), Operation :: commutative_associative_binary_operation()) -> t().
new(I, O) -> #commutative_monoid{identity = I, operation = O}.

-spec identity(t()) -> commutative_monoid_element().
identity(#commutative_monoid{identity = I}) -> I.

-spec operate(t(), commutative_monoid_element(), commutative_monoid_element()) -> commutative_monoid_element().
operate(#commutative_monoid{operation = O}, E1, E2) -> O(E1, E2).


-spec reduce(t(), sets:set(commutative_monoid_element())) -> commutative_monoid_element().
reduce(#commutative_monoid{identity = I, operation = O}, Es) -> sets:fold(O, I, Es).

-spec concurrent_reduce(t(), sets:set(commutative_monoid_element())) -> commutative_monoid_element().
concurrent_reduce(CM, Es) ->
    Me = self(),
    ReduceCoordinater = spawn(fun() -> reduce_loop(CM, sets:new(), false, Me) end),
    sets:fold(fun(E, _) -> ReduceCoordinater ! {Me, E} end, ok, Es),
    ReduceCoordinater ! done,
    receive Result -> Result end.
reduce_loop(#commutative_monoid{operation = O} = CM, ActiveWorkers0, HasReceivedAllWork, Caller) ->
    NActiveWorkers = sets:size(ActiveWorkers0),
    if
        HasReceivedAllWork andalso NActiveWorkers =:= 1 -> receive {_, Result} -> Caller ! Result end;
        true ->
            ReduceCoordinator = self(),
            receive
                done -> reduce_loop(CM, ActiveWorkers0, true, Caller);
                {Worker1, E1} ->
                    ActiveWorkers1 = sets:del_element(Worker1, ActiveWorkers0),
                    receive
                        done ->
                            NewWorker = spawn(fun() -> ReduceCoordinator ! {self(), E1} end),
                            ActiveWorkers2 = sets:add_element(NewWorker, ActiveWorkers1),
                            reduce_loop(CM, ActiveWorkers2, true, Caller);
                        {Worker2, E2} ->
                            ActiveWorkers2 = sets:del_element(Worker2, ActiveWorkers1),
                            NewWorker = spawn(fun() -> ReduceCoordinator ! {self(), O(E1, E2)} end),
                            ActiveWorkers3 = sets:add_element(NewWorker, ActiveWorkers2),
                            reduce_loop(CM, ActiveWorkers3, HasReceivedAllWork, Caller)
                    end
            end
    end.

-spec to_monoid(t()) -> monoid:t().
to_monoid(#commutative_monoid{identity = I, operation = O}) -> monoid:new(I, O).
