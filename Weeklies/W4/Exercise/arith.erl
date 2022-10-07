-module(arith).
-export([eval_simple/1]).

eval_simple({constant, Integer}) -> Integer;
eval_simple({add, Left, Right}) -> eval_simple(Left) + eval_simple(Right);
eval_simple({sub, Left, Right}) -> eval_simple(Left) - eval_simple(Right);
eval_simple({mul, Left, Right}) -> eval_simple(Left) * eval_simple(Right);
eval_simple({division, Left, Right}) -> eval_simple(Left) / eval_simple(Right).

