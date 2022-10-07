-module(hello).
-export([start/0]).

start() ->
    Pid = spawn(fun() -> server("Hello") end),
    Pid ! {world}.

server(Message) ->
    io:fwrite("~p", [Message]).
