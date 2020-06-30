-module(assignment).
-export([perimeter/1,area/1,enclose/1,bits/1,bits_tail/1]).

area({circle, {_X,_Y}, R}) -> math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) -> H*W;
area({triangle, {_X, _Y}, A, B, Hypotenuse}) -> A+B+Hypotenuse.

perimeter({circle, {_X,_Y}, R}) -> 2*math:pi()*R;
perimeter({rectangle, {_X,_Y}, H, W}) -> 2*H + 2*W;
perimeter({triangle, {_X,_Y}, A, B, _Hypotenuse}) -> A*B/2.

enclose({circle, {X, Y}, R}) -> {rectangle, {X, Y}, 2*R, 2*R};
enclose({rectangle, {X, Y}, H, W}) -> {rectangle, {X, Y}, H, W};
enclose({triangle, {X, Y}, H, W, _Hypotenuse}) -> {rectangle, {X, Y}, H, W}.

bits(0) -> 0;
bits(N) -> N rem 2 + bits(trunc(N/2)).

bits_tail(N) -> bits_tail(N, 0).
bits_tail(0,Count) -> Count;
bits_tail(N,Count) -> bits_tail(trunc(N/2),Count + N rem 2).

% bits_tail/1 is better because we never need to hold more than 2 terms in memory, so the space usage is constant.
% bits/1 on the other hand needs to keep an increasing number of values in memory.
