-module(abstract_algebra_examples).

-export([ sets_under_union/0
        , lists_under_concatenation/0
        , integers_under_addition/0
        , integers_under_multiplication/0
        ]).

-spec lists_under_concatenation() -> monoid:t().
lists_under_concatenation() -> monoid:new([], fun(L1, L2) -> L1 ++ L2 end).

-spec integers_under_addition() -> commutative_monoid:t().
integers_under_addition() -> commutative_monoid:new(0, fun(I1, I2) -> I1 + I2 end).

-spec integers_under_multiplication() -> commutative_monoid:t().
integers_under_multiplication() -> commutative_monoid:new(1, fun(I1, I2) -> I1 * I2 end).

-spec sets_under_union() -> commutative_monoid:t().
sets_under_union() -> commutative_monoid:new(sets:empty(), fun sets:union/2).
