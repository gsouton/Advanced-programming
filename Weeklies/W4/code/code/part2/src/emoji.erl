-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2, analytics/5,
         get_analytics/2, remove_analytics/3, stop/1, request/2, addAlias/4]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

% start(_) -> not_implemented.
start(Initial) ->
    %check content of Initial
    {ok, spawn(fun() -> loop(Initial, [], []) end)}.

request(E, Request) ->
    E ! {self(), Request},
    receive
        {E, Response} ->
            Response
    end.

look([], _) ->
    no_emoji;
look([{Key, Value} | T], Short) ->
    if Key =:= Short ->
           {ok, Value};
       true ->
           look(T, Short)
    end.

addEmoji(EmojiList, {Short, Code}) ->
    Check = look(EmojiList, Short),
    if Check =:= no_emoji ->
           {lists:append(EmojiList, [{Short, Code}]), ok};
       true ->
           {EmojiList, {error, emoji_exists}}
    end.

addAlias(EmojiList, AliasList, Short1, Short2) ->
    case look(AliasList, Short1) % check if short1 is already an alias
    of
        no_emoji -> %in case not alias look in the emoji list
            case look(EmojiList, Short1) of
                no_emoji ->
                    {AliasList, {error, no_emoji}};
                {ok, _} ->
                    {lists:append(AliasList, [{Short2, Short1}]), ok}
            end;
        {ok, Value} -> %case where Short1 is an alias
            {lists:append(AliasList, [{Short2, Value}]), ok}
    end.

isShortValid(EmojiList, AliasList, Short) ->
    case look(AliasList, Short) of
        no_emoji ->
            case look(EmojiList, Short) of
                no_emoji ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

registerAnalytics(EmojiList, AliasList, AnalyticsList, Short, Fun, Label, Init) ->
    case lists:any(fun({S, _, L, _}) -> (S =:= Short) and (L =:= Label) end, AnalyticsList)
    of
        true ->
            {AnalyticsList, {error, already_defined}};
        false ->
            {lists:append(AnalyticsList, [{Short, Fun, Label, Init}]), ok}
    end.

getAliases(AliasList, Short) ->
    list:map(fun({Key, Value}) -> if Short =:= Value -> Key end end, AliasList).

getAnalytics(_AliasList, AnalyticsList, Short) ->
    List = lists:filter(
             fun({S, _, _, _}) -> (S =:= Short) end, AnalyticsList),
    {ok, lists:map(fun({_, _, L, I}) -> {L, I} end, List)}.

lookup(EmojiList, AliasList, AnalyticsList, Short) ->
    case look(AliasList, Short) %checking if alias exists
    of
        no_emoji -> %no alias exist
            Res = look(EmojiList, Short),
            applyAnalytics(Res, AnalyticsList, Short);
        {ok, Value} -> %an alias exist, it might point to another alias
            lookup(EmojiList, AliasList, AnalyticsList, Value)
    end.

applyAnalytics(Res, AnalyticsList, Short) ->
    case Res of
        no_emoji ->
            {AnalyticsList, no_emoji};
        {ok, Value} ->
            {lists:map(fun({S, F, L, I}) ->
                          if Short =:= S -> {S, F, L, apply(F, [S, I])};
                             true -> {S, F, L, I}
                          end
                       end,
                       AnalyticsList),
             {ok, Value}}
    end.

loop(EmojiList, AliasList, AnalyticsList) ->
    receive
        % --- Adding an emoji
        {From, {add, Emoji}} ->
            {NewEmojiList, Res} = addEmoji(EmojiList, Emoji),
            From ! {self(), Res},
            loop(NewEmojiList, AliasList, AnalyticsList);
        % --- Adding an alias
        {From, {alias, {Short1, Short2}}} ->
            {NewAliasList, Res} = addAlias(EmojiList, AliasList, Short1, Short2),
            From ! {self(), Res},
            loop(EmojiList, NewAliasList, AnalyticsList);
        % --- Deleting an emoji
        {From, {delete, Short}} ->
            case look(AliasList, Short) %checking if short is alias
            of
                no_emoji ->
                    NewEmojiList = lists:filter(fun({Key, _}) -> Key =:= Short end, EmojiList),
                    From ! {self(), ok},
                    loop(NewEmojiList, AliasList, AnalyticsList);
                {ok, Value} ->
                    NewEmojiList = lists:filter(fun({Key, _}) -> Key =:= Value end, EmojiList),
                    NewAliasList = lists:filter(fun({_, V}) -> Value =:= V end, AliasList),
                    From ! {self(), ok},
                    loop(NewEmojiList, NewAliasList, AnalyticsList)
            end;
        % --- Looking up for emoji
        {From, {look, {Short}}} ->
            {NewAnalyticsList, Res} = lookup(EmojiList, AliasList, AnalyticsList, Short),
            From ! {self(), Res},
            loop(EmojiList, AliasList, NewAnalyticsList);
        % --- Adding analytics
        {From, {analytics, {Short, Fun, Label, Init}}} ->
            {NewAnalyticsList, Res} =
                registerAnalytics(EmojiList, AliasList, AnalyticsList, Short, Fun, Label, Init),
            From ! {self(), Res},
            loop(EmojiList, AliasList, NewAnalyticsList);
        % --- Looking up analytics
        {From, {get_analytics, {Short}}} ->
            Res = getAnalytics(AliasList, AnalyticsList, Short),
            From ! {self(), Res},
            loop(EmojiList, AliasList, AnalyticsList);
        % --- Stop
        {From, {stop}} ->
            From ! {self(), ok}
    end.

new_shortcode(E, Short, Emo) ->
    request(E, {add, {Short, Emo}}).

alias(E, Short1, Short2) ->
    request(E, {alias, {Short1, Short2}}).

delete(E, Short) ->
    request(E, {delete, {Short}}).

lookup(E, Short) ->
    request(E, {look, {Short}}).

analytics(E, Short, Fun, Label, Init) ->
    request(E, {analytics, {Short, Fun, Label, Init}}).

get_analytics(E, Short) ->
    request(E, {get_analytics, {Short}}).

remove_analytics(_, _, _) ->
    not_implemented.

stop(E) ->
    request(E, {stop}).
