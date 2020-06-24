-module(second).
-export([hypo/2,perim/2,area/2]).

hypo(X,Y) ->
  math:sqrt(first:square(X)+first:square(Y)).

perim(X,Y) ->
  X+Y+hyp(X,Y).

area(X,Y) ->
  X*Y/2.
