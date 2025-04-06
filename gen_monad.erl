-module(gen_monad).

-export([ return/2
        , bind/3
        ]).


-callback return(term()) -> term().
-callback bind(term(), fun((term()) -> term())) -> term().

-spec return(module(), term()) -> term().
return(Module, Term) ->
    Module:return(Term).

-spec bind(module(), term(), fun((term()) -> term())) -> term().
bind(Module, MonadInstance, Operation) ->
    Module:bind(MonadInstance, Operation).
