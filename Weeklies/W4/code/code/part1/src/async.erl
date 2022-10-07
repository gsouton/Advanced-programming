-module(async).

-export([new/2, wait/1, poll/1, hello/0, test/0]).

new(Fun, Arg) -> 
    spawn(?MODULE, Fun, Arg). 

wait(Aid) -> 
    receive
        {Aid, exception, Exception} -> throw(Exception);
        {Aid, ok, Response} -> Response,
        wait(Aid)
    end.

poll(Aid) ->
    receive
        {Aid, exception, Exception} -> Exception;
        {Aid, ok, Response} -> Response
    end.

hello() ->
    {self(), ok, "Hello World"}.

test() ->
    Aid = new(hello, []),
    io:write(Aid),
    poll(Aid).
