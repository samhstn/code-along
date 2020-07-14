-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1,compose_list/1,twice/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

% Takes a number N and returns a function that takes a function and returns that function
% iterated N times. When N is zero it returns the identity function.
iterate(N) -> fun(F) -> compose_list(lists:duplicate(N, F)) end.

% Instructed to write in the shell:
%
% 1> Add=fun(X,Y) -> X+Y end.
% #Fun<erl_eval.43.97283095>
% 2> Sum=fun(Xs) -> lists:foldr(Add,0,Xs) end.
% #Fun<erl_eval.44.97283095>
% 3> c(hof).
% hof.erl:19: Warning: variable 'N' is unused
% {ok,hof}
% 4> c(hof).
% {ok,hof}
% 5> EmptyTest = fun ([]) -> true ; ([_|_]) -> false end.
% #Fun<erl_eval.44.97283095>
% 6> Add(1,2).
% 3
% 7> Sum([3,4,67]).
% 74
% 8> EmptyTest([3,4,67]).
% false
% 9> Foo = fun Product([]) -> 1 ; Product([X|Xs]) -> X*Product(Xs) end.
% #Fun<erl_eval.20.97283095>
% 10> Foo([0,1,2]).
% 0
% 11> Foo([10,1,2]).
% 20
% 12> Product([10,1,2]).
% * 1: variable 'Product' is unbound

compose_list([F|Fs]) -> fun(X) -> F((compose_list(Fs))(X)) end;
compose_list([]) -> fun id/1.

twice(F) -> fun(X) -> F(F(X)) end.
