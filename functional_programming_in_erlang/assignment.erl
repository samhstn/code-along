-module(assignment).
-export([perimeter/1,area/1,enclose/1,bits/1]).

area({circle, {_X,_Y}, R}) -> math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) -> H*W.
% area({triangle, {_X, _Y}, })

perimeter({circle, {_X,_Y}, R}) -> 2*math:pi()*R;
perimeter({rectangle, {_X,_Y}, H, W}) -> 2*H + 2*W.

enclose({circle, {X, Y}, R}) ->
  {rectangle, {X, Y}, 2*R, 2*R};
enclose({rectangle, {X, Y}, H, W}) ->
  {rectangle, {X, Y}, H, W}.

bits(N) ->
  bits(N, 0).
bits(0,Count) ->
  Count;
bits(N,Count) when N rem 2 == 0 ->
  bits(trunc(N/2),Count);
bits(N,Count) when N rem 2 == 1 ->
  bits(trunc(N/2),Count+1).
