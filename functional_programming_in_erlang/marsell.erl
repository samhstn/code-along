-module(marsell).
-export([index/1]).

index(Path) ->
Lines = get_file_contents(Path),
Word_lines = lists:map(fun sentence_to_words/1, Lines),
Word_lineno = lines_to_word_lineno(Word_lines),
erlang:display(create_index(Word_lineno)).

% Convert a line of characters to a list of punctuation-free words, with
% short and common words removed
sentence_to_words(Line) ->
filter_stop_words(filter_short_words(split_words(normalize(Line)))).

% return list of words with words shorter than two characters removed
filter_short_words(Words) ->
lists:filter(fun(W) -> length(W) > 2 end, Words).

% return list of words with stop (common) words removed
filter_stop_words([]) -> [];
filter_stop_words([Word|T]) ->
Match = lists:member(Word, ["that", "the", "here", "and", "can", "for"]),
if Match -> filter_stop_words(T);
true -> [Word | filter_stop_words(T)]
end.

% convert string to lower case, and remove all punctuation
normalize([]) -> [];
normalize([H |T]) when H>96, H<123 -> [H | normalize(T)]; % lower case
normalize([H |T]) when H>64, H<91 -> [H+32 | normalize(T)]; % upper case
normalize([32|T]) -> [32 | normalize(T)]; % space char
normalize([_ |T]) -> normalize(T).

% split a string into a list of words
split_words([]) -> [];
split_words(L) -> split_words(L, []).
split_words([], A) -> [lists:reverse(A)];
split_words([32|T], A) -> [lists:reverse(A) | split_words(T, [])];
split_words([C |T], A) -> split_words(T, [C|A]).

% return list of {word, line} pairs, where each pair is a string word and the
% line it was found upon
lines_to_word_lineno(L) -> lines_to_word_lineno(L, 1).
lines_to_word_lineno([], _) -> [];
lines_to_word_lineno([Words|T], Line_no) ->
add_line_number(Words, Line_no) ++ lines_to_word_lineno(T, Line_no+1).

% take a list of words, and convert to a list of {word, line number} pairs
add_line_number(Words, Line_no) ->
lists:map(fun(Word) -> {Word, Line_no} end, Words).

% given a list of {word, line number} pairs, convert to a list of
% {word, [...line number...]} pairs
create_index(Word_lineno) ->
Sorted = lists:sort(fun word_sorter/2, Word_lineno),
compact_index(Sorted).

% sort a list of {word, line number} pairs, first lexicographically and then
% by line number
word_sorter({Word1,Line1}, {Word2,Line2}) when Word1 == Word2 -> Line1 < Line2;
word_sorter({Word1,_}, {Word2,_}) -> Word1 < Word2.

% take a sorted ilist of {word, line number} pairs, and create a list of
% {word, [...line number...]} pairs
compact_index([]) -> [];
compact_index(Word_lineno) ->
{Lookup, Remainder} = word_index(Word_lineno),
[Lookup | compact_index(Remainder)].

% given a specific word, generate {word, [...line number...]} pairs from
% a list of {word, line number} pairs, and also return any pairs that weren't
% that word
word_index([]) -> [];
word_index([{Word, _}=C | T]) ->
{Indexes, Remainder} = scan_index(C, C, T, []),
{{Word, lists:reverse(Indexes)}, Remainder}.

scan_index({_,Lineno1}, {_,Lineno2}, [], Acc) ->
{[{Lineno1, Lineno2} | Acc], []};
scan_index({Word, Lineno1}=S, {Word, Lineno2}, [{Word, Lineno3}=C|T], Acc) ->
if Lineno3 == Lineno2 + 1 ->
scan_index(S, C, T, Acc);
true ->
scan_index(C, C, T, [{Lineno1, Lineno2}|Acc])
end;
scan_index({Word, Lineno1}, {Word, Lineno2}, T, Acc) ->
{[{Lineno1, Lineno2}|Acc], T}.

% helpers given by course --------------

get_file_contents(Name) ->
{ok,File} = file:open(Name,[read]),
Rev = get_all_lines(File,[]),
lists:reverse(Rev).

get_all_lines(File,Partial) ->
case io:get_line(File,"") of
eof -> file:close(File),
Partial;
Line -> {Strip,_} = lists:split(length(Line)-1,Line),
get_all_lines(File,[Strip|Partial])
end.
