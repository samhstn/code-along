% ABC are lengths of the sides of a triangle
-module(matthew).
-export([area/1,peri/1,enclose/1]).

area({triangle, {X,Y}, A, B, C})
-> S = peri({triangle, {X,Y}, A, B, C})/2,
math:sqrt(S*(S-A)*(S-B)*(S-C));
area({circle, {_X,_Y}, R})
-> math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W})
-> H*W.

peri({triangle, {_X,_Y}, A, B, C})
-> A+B+C;
peri({circle, {_X,_Y}, R})
-> math:pi()*R*2;
peri({rectangle, {_X,_Y}, H, W})
-> (2*H)+(2*W).

enclose({circle, {X,Y}, R})
-> {rectangle, {X,Y}, 2*R, 2*R};
enclose({rectangle, {X,Y}, H, W})
-> {rectangle, {X,Y}, H, W}.
