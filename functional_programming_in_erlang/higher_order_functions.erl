-module(higher_order_functions).
-export([all_areas/1,circles/1,sum/1,doubleAll/1,evens/1,product/1,zip/2,zip_with/3,reduce/3]).

map(_F,[]) -> [];
map(F,[X|Xs]) -> [F(X)|map(F,Xs)].

filter(_P,[]) -> [];
filter(P,[X|Xs]) ->
  case P(X) of
    true -> [X|filter(P,Xs)];
    false -> filter(P,Xs)
  end.

reduce(_Combine, Start, []) ->
  Start;
reduce(Combine, Start, [X|Xs]) ->
  Combine(X, reduce(Combine, Start, Xs)).

all_areas(Shapes) -> map(fun assignment:area/1, Shapes).

is_circle({circle,{_,_},_}) -> true;
is_circle({rectangle,{_,_},_,_}) -> false.

circles(Shapes) -> filter(fun is_circle/1,Shapes).

sum(Xs) -> reduce(fun plus/2, 0, Xs).

plus(X,Y) -> X+Y.

doubleAll(Xs) -> map(fun(X) -> X*2 end, Xs).
evens(Xs) -> filter(fun(X) -> X rem 2 == 0 end, Xs).
product(Xs) -> reduce(fun(X,Y) -> X*Y end, 1, Xs).

zip([X|Xs], [Y|Ys]) -> [{X,Y}|zip(Xs,Ys)];
zip([],_Ys) -> [];
zip(_Xs,[]) -> [].

zip_with(F, Xs, Ys) -> lists:map(fun({X,Y}) -> F(X,Y) end, zip(Xs, Ys)).
