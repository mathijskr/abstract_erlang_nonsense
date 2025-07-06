-module(erlmonad).

-export([parse_transform/2]).

-define(BIND, '>=').

parse_transform(ASTIn, _Options) ->
    io:format("~p~n~n~n~n", [ASTIn]),
    ASTOut = lists:map(fun replace_monads/1, ASTIn),
    io:format("~p~n", [ASTOut]),
    ASTOut.

% TODO: mixed monads?
% NOTE: probably incomplete
replace_monads({FormInstance, Anno, Arg1, Arg2, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, Arg2, lists:map(fun replace_monads/1, Reprs)};
replace_monads({FormInstance, Anno, Arg1, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, lists:map(fun replace_monads/1, Reprs)};
replace_monads({tuple, _Anno, [{atom, _AnnoMonad, monad}, {atom, _AnnoMonadImpl, MonadImpl}, Expr]}) ->
    replace_binds(MonadImpl, Expr);
replace_monads(LetBe) -> LetBe.

replace_binds(MonadImpl, {op, Anno, ?BIND, LHS, RHS}) ->
    Fun = {remote, Anno, {atom, Anno, gen_monad}, {atom, Anno, bind}},
    Args = [{atom, Anno, MonadImpl}, replace_binds(MonadImpl, LHS), replace_binds(MonadImpl, RHS)],
    {call, Anno, Fun, Args};
replace_binds(MonadImpl, {FormInstance, Anno, Arg1, Arg2, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, Arg2, lists:map(fun(Repr) -> replace_binds(MonadImpl, Repr) end, Reprs)};
replace_binds(MonadImpl, {FormInstance, Anno, Arg1, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, lists:map(fun(Repr) -> replace_binds(MonadImpl, Repr) end, Reprs)};
replace_binds(MonadImpl, {FormInstance, Anno, {clauses, Reprs}}) when is_list(Reprs) ->
    {FormInstance, Anno, {clauses, lists:map(fun(Repr) -> replace_binds(MonadImpl, Repr) end, Reprs)}};
replace_binds(_MonadImpl, LetBe) -> LetBe.
