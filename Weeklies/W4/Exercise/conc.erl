-module(conc).
-export([hello/0, create/1]).

hello() -> 
    Me = self(),
    "Hello: " ++ Me.

create(0, List) -> 
    List;

create(N, List) ->
    Me = self(),
    _Ch1 = spawn(fun() -> Me ! hello() end),
    receive
        Pat -> List ++ Pat;
    end.
    create(N-1, List).


