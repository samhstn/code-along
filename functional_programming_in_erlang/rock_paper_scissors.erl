-module(rock_paper_scissors).
-import(higher_order_functions, [reduce/3,zip/2]).
-export([beat/1,lose/1,result/2,tournament/2]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(scissors) -> paper;
lose(paper) -> rock.

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;

result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;

result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

outcome(win) -> 1;
outcome(lose) -> -1;
outcome(draw) -> 0.

tournament(Rounds1,Rounds2) ->
  reduce(fun({X,Y},Acc) -> Acc+outcome(result(X,Y)) end, 0, zip(Rounds1,Rounds2)).
