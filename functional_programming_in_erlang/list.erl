-module(list).
-export([head/1,tail/1,second/1,sum/1,sum_tail/1,prod/1,prod_tail/1,maximum/1,maximum_tail/1,median/1,mode/1,double/1,evens/1,take/2,nub/1,palindrome/1,member/2,concat/1]).

% every list can be built up from [] using the constructor [...|...] - called the cons (constructor) operator.

head([X|_Xs]) -> X.
tail([_X|Xs]) -> Xs.
second([_X,Y|_Xs]) -> Y.

sum([]) -> 0;
sum([X|Xs]) -> X + sum(Xs).

sum_tail(Xs) -> sum_tail(Xs,0).
sum_tail([],S) -> S;
sum_tail([X|Xs],S) -> sum_tail(Xs,S+X).

% prod/1 of an empty list is taken to be 1, as in multiplication 1 is the identity or the base case.
prod([]) -> 1;
prod([X|Xs]) -> X * prod(Xs).

prod_tail(Xs) -> prod_tail(Xs, 1).
prod_tail([],P) -> P;
prod_tail([X|Xs],P) -> prod_tail(Xs, P*X).

maximum([X]) -> X;
maximum([X|Xs]) -> max(X,maximum(Xs)).

maximum_tail([X|Xs]) -> maximum_tail(Xs,X).
maximum_tail([],X) -> X;
maximum_tail([X|Xs],Y) -> maximum_tail(Xs,max(X,Y)).

% todo
median(_Xs) -> 1.

% todo
mode(_Xs) -> 1.

-spec double([T]) -> [T].
double([]) -> [];
double([X|Xs]) -> [2*X|double(Xs)].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 -> [X|evens(Xs)];
evens([X|Xs]) when X rem 2 == 1 -> evens(Xs).

member([],_El) -> false;
member([X|_Xs],X) -> true;
member([_X|Xs],El) -> member(Xs,El).

-spec take(integer(), [T]) -> [T].
take(_Xs,0) -> [];
take([],_N) -> [];
take([X|Xs],N) ->
  [X|take(Xs,N-1)].

nub([]) ->
  [];
nub([X|Xs]) ->
  [X|nub(removeAll(X,Xs))].

removeAll(_X, []) ->
  [];
removeAll(X, [X|Xs]) ->
  removeAll(X, Xs);
removeAll(X, [Y|Xs]) ->
  [Y | removeAll(X, Xs)].

% palindrome("Madam I\'m Adam.") = true
palindrome(Xs) ->
  palin(nocaps(nopunct(Xs))).

nopunct([]) ->
  [];
nopunct([X|Xs]) ->
  case member("'.,\ ;:\t\n",X) of
    true -> nopunct(Xs);
    false -> [X|nopunct(Xs)]
  end.

nocaps([]) ->
  [];
nocaps([X|Xs]) ->
  [nocap(X)|nocaps(Xs)].

nocap(X) ->
  case $A =< X andalso X =< $Z of
    true ->
      X+32;
    false ->
      X
  end.

palin(Xs) ->
  Xs == reverse(Xs).

reverse(Xs) ->
  shunt(Xs, []).

shunt([], Ys) ->
  Ys;
shunt([X|Xs],Ys) ->
  shunt(Xs,[X|Ys]).

concat([]) ->
  "";
concat([X|Xs]) ->
  X ++ concat(Xs).
