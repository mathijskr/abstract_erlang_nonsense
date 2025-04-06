-module(state_monad_example).

-export([do/0]).

-import(state_monad, [return/1, bind/2, puts/1, gets/0, run/2, exec/2, eval/2]).

do() ->
    Init = 4242,
    eval(bind(           gets()
      , fun(A) -> bind(  puts(A + 1)
      , fun(_) -> bind(  gets()
      , fun(B) ->        return([A, B])
        end) end) end)
    , Init).
