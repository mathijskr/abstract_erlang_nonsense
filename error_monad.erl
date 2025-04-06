-module(error_monad).

-behavior(gen_monad).

-export([ bind/2
        , return/1
        ]).

-export([ error/1
        , ok/1
        ]).

-type t() :: {error, term()} | {ok, term()}.

-spec ok(term()) -> {ok, term()}.
ok(Term) -> {ok, Term}.

-spec error(term()) -> t().
error(Term) -> {error, Term}.

-spec return(term()) -> {ok, term()}.
return(T) -> ok(T).

-spec bind(t(), fun((term()) -> t())) -> t().
bind({error, _} = E, _Operation) -> E;
bind({ok, Term}, Operation) -> Operation(Term).


% Proof error_monad satisfies monad laws:
% return is left identity w.r.t. bind:
%   bind(return(T), F) =
%   bind({ok, T}, F)   =
%   F(T)
% q.e.d.
%
% return is right identity w.r.t. bind:
%   bind(M, fun(T) -> return(T) end) =
%   | (case M = {error, E}):
%       bind({error, E}, fun(T) -> return(T) end) =
%       {error, E} = M
%       q.e.d.
%   | (case M = {ok, T}):
%       bind({ok, T}, fun(T) -> return(T) end) =
%       (fun(T) -> return(T) end)(T) =
%       {ok, T} = M
%       q.e.d.
%   q.e.d.
%
% bind is associative:
% bind(bind(bind(M, F), G), H) = bind(bind(M, F), fun(X) -> bind(G(X), H) end)
% | (case M = {error, E}):
%     bind(bind(bind({error, E}, F), G), H) = bind(bind({error, E}, F), fun(X) -> bind(G(X), H) end) =
%     {error, E}                            = {error, E}
%     q.e.d.
% | (case M = {ok, T}):
%     bind(bind(bind({ok, T}, F), G), H) = bind(bind({ok, T}, F), fun(X) -> bind(G(X), H) end) =
%     bind(bind(F(T), G), H)             = bind(F(T), fun(X) -> bind(G(X), H) end)
%     | (case F(T) = {error, E}):
%         bind(bind({error, E}, G), H) = bind({error, E}, fun(X) -> bind(G(X), H) end) =
%         {error, E}                   = {error, E}
%         q.e.d.
%     | (case F(T) = {ok, U}):
%         bind(bind({ok, U}, G), H) = bind({ok, U}, fun(X) -> bind(G(X), H) end) =
%         bind(G(U), H) = bind(G(U), H)
%         q.e.d.
%   q.e.d.
% q.e.d.
