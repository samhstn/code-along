-module(recursion).
-export([fac/1,fib/1,twoDPieces/1,loop/1,perfect/1]).

fac(0) ->
  1;
fac(N) when N>0 ->
  fac(N-1)*N.

fib(1) ->
  0;
fib(2) ->
  1;
fib(N) when N>2 ->
  fib(N-1) + fib(N-2).

twoDPieces(0) ->
  2;
twoDPieces(Cuts) when Cuts>0 ->
  Cuts+twoDPieces(Cuts-1).

loop(N) when N>0 ->
  io:format("~p~n",[N]),
  loop(N-1);
loop(_) ->
  io:format("bye~n").

perfect(N) when N>0 ->
  perfect(N,[1],2).
perfect(N,Divisors,Count) when Count >= N ->
  io:fwrite("Divisors: ~p~n",[Divisors]),
  lists:sum(Divisors) == N;
perfect(N,Divisors,Count) when N rem Count == 0 ->
  perfect(N,[Count|Divisors],Count+1);
perfect(N,Divisors,Count) when N rem Count /= 0 ->
  perfect(N,Divisors,Count+1).
