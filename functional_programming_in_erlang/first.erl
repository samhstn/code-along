-module(first).
-export([double/1,treble/1,mult/2,square/1,area/3,xOr/2,maxThree/3,howManyEqual/3]).

mult(X,Y) ->
  X*Y.

double(X) ->
  mult(2,X).

treble(X) ->
  mult(3,X).

square(X) ->
  mult(X,X).

area(A,B,C) ->
  S = (A+B+C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

xOr(X,X) ->
  false;
xOr(_,_) ->
  true.

maxThree(X,Y,Z) ->
  max(max(X,Y),Z).

howManyEqual(X,X,X) -> 3;
howManyEqual(X,_,X) -> 2;
howManyEqual(X,X,_) -> 2;
howManyEqual(_,X,X) -> 2;
howManyEqual(_,_,_) -> 0.
