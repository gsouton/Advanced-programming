-module(love_emoji).
-export([try_it/0]).

hit(_, N) -> N+1.
accessed(SC, TS) ->
  Now = calendar:local_time(),
  [{SC,Now} | TS].

setup() ->
    {ok, E} = emoji:start([]),
    ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
    ok = emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>),
    ok = emoji:alias(E, "poop", "hankey"),
    ok = emoji:alias(E, "hankey", "poo"),
    ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
    % ok = emoji:analytics(E, "hankey", fun hit/2, "Counter", 0),
    % ok = emoji:analytics(E, "poop", fun accessed/2, "Accessed", []),
    E.

% print_analytics(Stats) ->
%     lists:foreach(fun({Lab, Res}) -> io:fwrite("  ~s: ~p~n", [Lab, Res]) end,
%                   Stats).

try_it() ->
    E = setup(),
    {ok, Res} = emoji:lookup(E, "smiley"),
    emoji:lookup(E, "poo"),
    io:fwrite("I looked for :hankey: and got a pile of ~ts~n", [Res]).
    % emoji:get_analytics(E, "smiley").
    % {ok, Stats} = emoji:get_analytics(E, "poop"),
    % io:fwrite("Poppy statistics:~n"),
    % print_analytics(Stats),
    % io:fwrite("(Hopefully you got a 1 under 'Counter')~n").
