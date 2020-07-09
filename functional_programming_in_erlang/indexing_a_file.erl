-module(indexing_a_file).
-include_lib("eunit/include/eunit.hrl").
-export([main/1,parse/1,get_file_contents/1,show_file_contents/1]).

% run in an erl shell with:
% > c(indexing_a_file).
% > indexing_a_file:test().
% > indexing_a_file:main("gettysburg-address.txt").

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
      Partial;
    Line -> {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.
     
main(Name) ->
  parse(get_file_contents(Name)).

parse(FileContents) ->
  WithIndex=add_index(FileContents, 1),
  SplitWords=lists:flatten(lists:map(fun({Line, Index}) -> lists:map(fun(Word) -> {Word, Index} end, string:split(Line, " ", all)) end, WithIndex)),
  GroupWords=group_words(SplitWords),
  lists:map(fun({Word, Indexes}) -> {Word, group_indexes(Indexes)} end, GroupWords).

add_index([Head|Tail], Count) ->
  [{Head,Count}|add_index(Tail,Count+1)];
add_index([], _) ->
  [].

group_words(WordElems) -> group_words(WordElems, []).
group_words([Head|Tail], Acc) ->
  group_words(Tail, place_word(Head, Acc));
group_words([], Acc) -> Acc.

place_word(WordElem, WordElems) -> place_word(WordElem, WordElems, []).
place_word({Word, Index}, [{Word, Indexes}|Tail], Acc) ->
  lists:reverse([{Word, Indexes ++ [Index]}|Acc]) ++ Tail;
place_word({Word, Index}, [WordElem|Tail], Acc) ->
  place_word({Word, Index}, Tail, [WordElem|Acc]);
place_word({Word, Index}, [], Acc) -> lists:reverse([{Word, [Index]} | Acc]).

group_indexes(Indexes) -> group_indexes(Indexes, []).
group_indexes([Head|Tail], []) -> group_indexes(Tail, [{Head, Head}]);
group_indexes([Head|Tail], [{_, Head}|_] = Acc) -> group_indexes(Tail, Acc);
group_indexes([Head|Tail], [{First, Last}|TailAcc]) when Head == Last + 1 ->
  group_indexes(Tail, [{First, Head}|TailAcc]);
group_indexes([Head|Tail], Acc) ->
  group_indexes(Tail, [{Head, Head}|Acc]);
group_indexes([], Acc) -> lists:reverse(Acc).

parse_file_contents_test() ->
  ?assertEqual(
    parse(["hello world", "one two three bye", "yo two", "hello bye"]),
    [{"hello",[{1,1},{4,4}]},
     {"world",[{1,1}]},
     {"one",[{2,2}]},
     {"two",[{2,3}]},
     {"three",[{2,2}]},
     {"bye",[{2,2},{4,4}]},
     {"yo",[{3,3}]}
    ]
  ).
