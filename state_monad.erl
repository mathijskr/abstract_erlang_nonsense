-module(state_monad).

-behavior(gen_monad).


-export([ return/1
        , bind/2
        ]).

-export([ puts/1
        , gets/0
        , run/2
        , exec/2
        , eval/2
        ]).


-type return() :: term().
-type state() :: term().
-type t() :: fun((term()) -> {return(), state()}).

-spec return(return()) -> t().
return(Return) ->
    fun(State) -> {Return, State} end.

-spec bind(t(), fun((return()) -> t())) -> t().
bind(StateMonad, Operation) ->
    fun(InitState) ->
        {Return, OldState} = StateMonad(InitState),
        run(Operation(Return), OldState)
    end.


-spec puts(state()) -> t().
puts(NewState) ->
    fun(_OldState) -> {undefined, NewState} end.

-spec gets() -> t().
gets() ->
    fun(State) -> {State, State} end.

-spec run(t(), term()) -> {return(), state()}.
run(StateMonad, InitState) ->
    StateMonad(InitState).

-spec exec(t(), state()) -> state().
exec(StateMonad, InitState) ->
    element(2, run(StateMonad, InitState)).

-spec eval(t(), state()) -> return().
eval(StateMonad, InitState) ->
    element(1, run(StateMonad, InitState)).
