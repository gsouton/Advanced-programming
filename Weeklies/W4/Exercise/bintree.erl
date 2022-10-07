-module(bintree).
-export([create_dict/0, insert/3, get/2]).

create_dict() -> leaf.

insert(leaf, Key, Value) -> {node, Key, Value, leaf, leaf};
insert({node, Tk, Tv, Left, Right}, Key, Value) ->
    if Key =:= Tk -> {node, Tk, Tv, Left, Right};
       Key > Tk -> {node, Tk, Tv, Left, insert(Right, Key, Value)};
       Key < Tk -> {node, Tk, Tv, insert(Left, Key, Value), Right}
    end.

get(leaf, _) -> none;
get({node, Tk, Tv, Left, Right}, Key) ->
    if Key =:= Tk -> Tv;
       Key > Tk -> get(Right, Key);
       Key < Tk -> get (Left, Key)
    end.
