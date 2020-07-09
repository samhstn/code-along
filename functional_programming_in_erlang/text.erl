-module(text).
-export([rio/0,space/1,words/1,drop_space/1,take_word/1,drop_word/1,get_line/2]).

% can be run with:
% > c(text).
% > text:words(text:rio()).

rio() -> "The heat bloomed                  in December\n" ++
 " as the      carnival  season\n" ++
 "                 kicked into gear.   \n" ++
 "Nearly helpless with sun and glare, I avoided Rio's brilliant \n" ++
 "sidewalks\n" ++
 " and glittering beaches,\n" ++
  "panting in dark     corners\n" ++
"  and waiting out the inverted southern summer.\n".

space(C) ->
    lists:member(C,"\t\n ").

% Word examples: "The"  "gear."

% take the longest word at the beginning of a string
% drop the longest word at the beginning of a string
% drop any space at the beginning of a string

% " the heat was …"
% remove space at the beginning
% "the heat was …"
% take the word at the beginning: "the"
% " heat was …"
% "heat was …"

words([]) -> [];
words(Cs) ->
    Xs = drop_space(Cs),
    [ take_word(Xs) | words(drop_word(Xs)) ].

drop_space([]) -> [];
drop_space([C|Cs]) ->
    case space(C) of
        true -> drop_space(Cs);
        _    -> [C|Cs]
    end.

drop_word([]) -> [];
drop_word([C|Cs]) ->
    case space(C) of
        true -> [C|Cs];
        _    -> drop_word(Cs)
    end.

take_word([]) -> [];
take_word([C|Cs]) ->
    case space(C) of 
        false -> [ C | take_word(Cs) ];
        true  -> []
    end.

get_line([],_) -> [];
get_line([W|Ws],N) ->
    L = length(W),
    case L>N of 
        true -> [];
        false -> [ W | get_line(Ws,N-L-1)]
    end.

% drop_line - corresponding to get_line

% split_line - get and drop line in a single function.

% sjdhfhs
% jkhjkjkjhgh
% jkhkhkh

%     sjdhfhs
% jkhjkjkjhgh
%     jkhkhkh


