-module(list_of_shapes).
-export([circles/1,total_area/1]).

all_areas([]) -> [];
all_areas([X|Xs]) -> [shape:area(X) | all_areas(Xs)].

total_area(Shapes) ->
  sum(all_areas(Shapes)).

circles([]) -> [];
circles([X|Xs]) ->
  case X of
    {circle, {_,},_} = C ->
      [C | circles(Xs)];
    _ ->
      circles(Xs)
  end
