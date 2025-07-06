-module(state_monad_example_pt).

-export([do/0]).

-compile({parse_transform, erlmonad}).

-import(state_monad, [return/1, puts/1, gets/0, run/2, exec/2, eval/2]).

do() ->
    Init = 4242,
    {monad, state_monad, eval( gets()
                            >= fun(A) -> puts(A + 1)
                            >= fun(_) -> gets()
                            >= fun(B) -> return([A, B])
                            end end end, Init)}.
