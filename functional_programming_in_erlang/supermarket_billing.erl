-module(supermarket_billing).
-export([database/0,my_codes/0,make_bill/2,lookup/2,lookup/1]).

-type item() :: string().
-type cost() :: integer().
-type code() :: integer().
-type codes() :: list(code()).
-type database() :: list({code(),item(),cost()}).
-type bill() :: list({item(),cost()}).

-spec my_codes() -> codes().
my_codes() -> [1234,4719,3814,1112,1113,1234].

database() ->
  [ {4719, "Fish Fingers", 121},
    {5643, "Nappies", 1010},
    {3814, "Orange Jelly", 56},
    {1111, "Hula Hoops", 21},
    {1112, "Hula Hoops (Giant}", 133},
    {1234, "Dry Sherry, 1lt", 540}].

-spec lookup(code(),database()) -> {item(),cost()}.
lookup(Code,[{Code,Item,Cost}|_Tail]) ->
  {Item,Cost};
lookup(Code,[_Head|Tail]) ->
  lookup(Code,Tail);
lookup(_Code,[]) ->
  {"Unknown Item", 0}.

-spec lookup(code()) -> {item(),cost()}.
lookup(Code) -> lookup(Code,database()).

-spec format_cost(integer()) -> any().
format_cost(N) when N<100 ->
  "0."++string:pad(integer_to_list(N), 2, leading, "0");
format_cost(N) when N>=100 ->
  integer_to_list(N div 100)++"."++string:pad(integer_to_list(N rem 100), 2, leading, "0").

-spec total(bill()) -> cost().
total([{_Item,Cost}|Tail]) ->
  Cost + total(Tail);
total([]) -> 0.

make_bill(Codes,Database) ->
  Bill=lists:map(fun(Code) -> lookup(Code,Database) end, Codes),

  io:fwrite("         Erlang Stores~n"),
  io:fwrite("~n"),

  lists:foreach(
    fun({Item,Cost}) ->
      io:format("~s~s~n", [string:pad(Item, 26, trailing, "."), format_cost(Cost)])
    end,
    Bill
  ),

  io:fwrite("~n"),
  io:format("~s~s~n", [string:pad("Total", 26, trailing, "."), format_cost(total(Bill))]).
