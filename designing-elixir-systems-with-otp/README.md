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
