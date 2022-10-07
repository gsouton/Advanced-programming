-module(ex).
-export([move/2, ignore_invalid/2, debug_move/2]).

move(south, {_, 0}) -> throw(invalid_move);
move(west, {0, _}) -> throw(invalid_move);
move(north, {X, Y}) -> {X, Y+1};
move(south, {X, Y}) -> {X, Y-1};
move(west, {X, Y}) -> {X-1, Y};
move(east, {X, Y}) -> {X+1, Y}.

ignore_invalid(Dir, Pos) ->
    try move(Dir, Pos)
    catch 
        invalid_move -> Pos
    end.

debug_move(Dir, Pos) ->
    try move(Dir, Pos)
    catch 
        invalid_move -> "Out of bounds"
    end.
