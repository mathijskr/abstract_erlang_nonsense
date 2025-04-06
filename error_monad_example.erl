-module(error_monad_example).

-export([do/0]).

do() ->
    Checks = [ fun check_zablessail/1
             , fun test_deywablell/1
             , fun prepare_fablenards/1
             ],
    InitValue = monad:return(error_monad, undefined),
    lists:foldl(fun(Check, Acc) -> monad:bind(error_monad, Acc, Check) end, InitValue, Checks).


% With help from feldarkrealms.com
check_zablessail(_Ignore) ->
    monad:return(error_monad, 42).

test_deywablell(_) ->
    Rand = rand:normal(),
    if
        Rand > 0 -> monad:return(error_monad, Rand);
        true -> error_monad:error({?FUNCTION_NAME, failed, Rand})
    end.

prepare_fablenards(SomeValue) ->
    monad:return(error_monad, {sdranelbaf, SomeValue}).
