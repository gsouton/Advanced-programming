-module(tree).

-export([eq_tree/2, eq_tree_list/4, list_tree/2, eq_tree_conc/2]).

eq_tree(leaf, leaf) ->
    true;
eq_tree(_, leaf) ->
    false;
eq_tree(leaf, _) ->
    false;
eq_tree({node, Value1, Left1, Right1}, {node, Value2, Left2, Right2}) ->
    if Value1 /= Value2 ->
           false;
       Value1 == Value2 ->
           eq_tree(Left1, Left2) and eq_tree(Right1, Right2)
    end.

eq_tree_list({node, Value1, Left1, Right1},
             {node, Value2, Left2, Right2},
             Tree1,
             Tree2) ->
    eq_tree_list(Left1, Left2, Tree1 ++ [Value1], Tree2 ++ [Value2])
    and eq_tree_list(Right1, Right2, Tree1 ++ [Value1], Tree2 ++ [Value2]);
eq_tree_list(leaf, leaf, Tree1, Tree2) ->
    if Tree1 =:= Tree2 ->
           true;
       Tree1 /= Tree2 ->
           false
    end;
eq_tree_list(_, leaf, _, _) -> false;
eq_tree_list(leaf, _, _, _) -> false.

list_tree(leaf, List) -> List ++ [leaf];
list_tree({node, V, L, R}, List) ->
    list_tree(L, List ++ [V]) ++ list_tree(R, List).


eq_tree_conc(T1, T2) ->
    Me = self(),
    _Process1 = spawn(fun() -> Me ! list_tree(T1, []) end),
    _Process2 = spawn(fun() -> Me ! list_tree(T2, []) end),
    receive
        List1 ->
            receive
                List2 -> List1 =:= List2
            end
    end.





