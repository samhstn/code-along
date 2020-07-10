-module(list_of_shapes).
-import(assignment,[area/1]).
-export([circles/1,total_area/1,all_areas/1]).

all_areas([]) -> [];
all_areas([X|Xs]) -> [assignment:area(X) | all_areas(Xs)].

total_area(Shapes) ->
  lists:sum(all_areas(Shapes)).

circles([]) -> [];
circles([X|Xs]) ->
  case X of
    {circle, {_,_},_} = C ->
      [C | circles(Xs)];
    _ ->
      circles(Xs)
  end.
