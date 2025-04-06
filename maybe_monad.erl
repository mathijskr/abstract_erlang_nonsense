-module(maybe_monad).

-behavior(gen_monad).

-export([ return/1
        , bind/2
        ]).

-export([ nothing/0
        ]).

-type t() :: nothing | {just, term()}.

-spec return(term()) -> t().
return(Term) -> {just, Term}.

% NOTE: operations should only return the atom `nothing` if the caller means to short-circuit the maybe monad.
-spec bind(t(), fun((term()) -> t())) -> t().
bind(nothing, _Operation) -> nothing;
bind({just, Term}, Operation) -> Operation(Term).

-spec nothing() -> nothing.
nothing() -> nothing.


% Proof maybe_monad satisfies monad laws:
% return is left identity w.r.t. bind:
% bind(return(T), F) =
% bind({just, T}, F) =
% F(T)
% q.e.d.
%
% return is right identity w.r.t. bind:
% bind(M, fun(T) -> return(T) end) =
% | (case M = nothing): bind(nothing, fun(T) -> return(T) end) =
%     nothing = M
%     q.e.d.
% | (case M = {just, T}): bind({just, T}, fun(T) -> return(T) end) =
%     return(T) = {just, T} = M
%     q.e.d.
%
% bind is associative:
% bind(bind(bind(M, F), G), H) = bind(bind(M, F), fun(X) -> bind(G(X), H) end)
% | (case M = nothing):
%     bind(bind(bind(nothing, F), G), H) = bind(bind(nothing, F), fun(X) -> bind(G(X), H) end) =
%     nothing = nothing
%     q.e.d.
% | (case M = {just, T}):
%     bind(bind(bind({just, T}, F), G), H) = bind(bind({just, T}, F), fun(X) -> bind(G(X), H) end) =
%     bind(bind(F(T)), G), H) = bind(F(T), fun(X) -> bind(G(X), H) end) =
%     | (case F(T) = nothing):
%         bind(bind(nothing), G), H) = bind(nothing, fun(X) -> bind(G(X), H) end) =
%         nothing = nothing
%         q.e.d.
%     | (case (F(T)) = {just, U}):
%         bind(bind({just, U}), G), H) = bind({just, U}, fun(X) -> bind(G(X), H) end) =
%         bind(G(U), H) = bind(G(U), H)
%         q.e.d.
%   q.e.d.
% q.e.d.
