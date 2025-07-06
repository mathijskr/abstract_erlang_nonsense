-module(erlmonad).

-export([parse_transform/2]).


parse_transform(ASTIn, _Options) ->
    lists:map(fun replace_monads/1, ASTIn).

% NOTE: probably incomplete
replace_monads({FormInstance, Anno, Arg1, Arg2, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, Arg2, lists:map(fun replace_monads/1, Reprs)};
replace_monads({FormInstance, Anno, Arg1, Reprs}) when is_list(Reprs) ->
    {FormInstance, Anno, Arg1, lists:map(fun replace_monads/1, Reprs)};

replace_monads({tuple, _Anno, [{atom, _AnnoMonad, do}, {atom, _AnnoMonadImpl, MonadImpl}, {block, _AnnoBlock, MonadBlock}]}) ->
    ReversedBlock = lists:reverse(MonadBlock),
    lists:foldl(fun({match, MatchAnno, MatchVar, MatchExpr}, AccExpr) ->
                        MonadBind = {remote, MatchAnno, {atom, MatchAnno, gen_monad}, {atom, MatchAnno, bind}},
                        FirstArg = {atom, MatchAnno, MonadImpl},
                        SecondArg = MatchExpr,
                        ThirdArg = {'fun', MatchAnno, {clauses, [{clause, MatchAnno, [MatchVar], [], [AccExpr]}]}},
                        BindArgs = [FirstArg, SecondArg, ThirdArg],
                        {call, MatchAnno, MonadBind, BindArgs}
                end, hd(ReversedBlock), tl(ReversedBlock));

replace_monads(LetBe) -> LetBe.
