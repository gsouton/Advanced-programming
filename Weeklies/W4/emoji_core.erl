-module(emoji_core).
-export([addEmoji/2, look/2, addAlias/4, test_add/0]).

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
    CheckEmoji = look(EmojiList, Short1),
    CheckAlias = look(AliasList, Short1),
    if CheckEmoji =:= no_emoji ->
           {AliasList, {error, no_emoji}};
       true ->
           if CheckAlias =:= no_emoji ->
                  {lists:append(AliasList, [{Short2, Short1}]), ok};
              true ->
                  {AliasList, {error, alias_exist}}
           end
    end.


test_add() ->
    ExpectedList = [{"smiley", <<240,159,152,131>>},{"poop", <<"\xF0\x9F\x92\xA9">>}],
    EmojiList = [],
    addEmoji(EmojiList, {"smiley", <<240,159,152,131>>}),
    addEmoji(EmojiList, {"poop", <<"\xF0\x9F\x92\xA9">>}),
    addEmoji(EmojiList, {"poop", <<"\xF0\x9F\x92\xA9">>}),
    addEmoji(EmojiList, {"smiley", <<240,159,152,131>>}),
    ExpectedList =:= EmojiList.

test_add_alias() ->
    EmojiList = [{"smiley", <<240,159,152,131>>},{"poop", <<"\xF0\x9F\x92\xA9">>}],
    AliasList = [],
    ExpectedList = [],
    addAlias(EmojiList, AliasList, Short1, Short2) ->
























