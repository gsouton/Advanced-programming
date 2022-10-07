-module(fib).
-export([fib/1]).

fib(0) -> 1;
fib(1) -> 1;

fib(N) ->
    Me = self(),
    _Ch1 = spawn(fun() -> Me ! fib(N-1) end),
    _Ch2 = spawn(fun() -> Me ! fib(N-2) end),
    receive
        N1 -> 
            receive
                  N2 -> N1 + N2
            end  
    end.

