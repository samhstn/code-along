### Ch 1

A boundary layer is:
+ The machinery of processes, message passing, and recursion that form the heart of concurrency in Elixir systems.
+ An API of plain functions that hides that machinery from clients.

### Ch 2

In Elixir, random access in lists is quite slow - O(n).
But random access and updating in maps is fast - O(log n).

Pattern matching a map or a struct is O(log n).

Don't count on ordering in maps!

A `MapSet` is the elixir version of sets.

Strings are to be used in user-defined text and atoms for naming comcepts in code.

Check out Connascence: https://connascence.io - very interesting.

Check out the actor model in Elixir - https://www.brianstorti.com/the-actor-model

"Don't send data to the functions because that's slow. Send the functions to the data!"

If we're working with arrays and need to update frequently and randomly, consider integrating a third-party solution into your program. - See the book "Adopting Elixir" for techniques here.

BEAM gives us a toolkit, so typically we don't need as many external dependencies as other environments.
So, we don't need memcached or Redis. A worker pool, or background job system can be created in 100 lines of code.
Or we can use libraries.

### Ch 3

We can use the `Access` module for updating nested data structures.

For tic-tac-toe, a great data structure for reads and writes is:

```elixir
%{
  {0,0}=>"O", {0,1}=>" ", {0,2}=>" ",
  {1,0}=>" ", {1,1}=>"X", {1,2}=>" ",
  {2,0}=>" ", {2,1}=>" ", {2,2}=>" "
}
```

Now reads and writes are trivial:

```iex
iex> board[{1,1}]
"X"
iex> Map.put(board, {1, 0}, "0")
```
The rule here is to prefer flat data structures to deep ones.

For bank account data structures, instead of updating an account balance, look to store all account transactions,
and all get requests should receive all (or all since a specific point in time).
