-module(higher_order_shapes).
-export([all_areas/1,circles/1,sum/1]).

map(_F,[]) -> [];
map(F,[X|Xs]) -> [F(X)|map(F,Xs)].

filter(_P,[]) -> [];
filter(P,[X|Xs]) ->
  case P(X) of
    true -> [X|filter(P,Xs)];
    false -> filter(P,Xs)
  end.

all_areas(Shapes) -> map(fun assignment:area/1, Shapes).

is_circle({circle,{_,_},_}) -> true;
is_circle({rectangle,{_,_},_,_}) -> false.

circles(Shapes) -> filter(fun is_circle/1,Shapes).

reduce(_Combine, Start, []) ->
  Start;
reduce(Combine, Start, [X|Xs]) ->
  Combine(X, reduce(Combine, Start, Xs)).

sum(Xs) -> reduce(fun plus/2, 0, Xs).

plus(X,Y) -> X+Y.
