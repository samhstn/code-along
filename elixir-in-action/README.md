# Elixir in Action

Erlang very good at transmitting frequent, real time updates to many connected users.

Elixir is a modern functional programming language for building large-scale scalable, distributed, fault-tolerant systems for the Erlang virtual machine.
Arguably Elixirs biggest advantage is that it targets the Erlang platform.

Erlang was made to help developers deal with the challenge of high availability.
Originally the product was intended for developing telecommunication systems,
but today it’s used in all kinds of domains, such as:
+ collaboration tools.
+ online payment systems.
+ real-time bidding systems.
+ database servers.
+ multiplayer online games.

Elixir aims to modernize and improve the experience of developing Erlang-powered systems.
Having the Erlang runtime as the target platform means Elixir-based systems are able to use all the libraries from the Erlang ecosystem,
including the battle-tested OTP frame work that ships with Erlang.

## 1 First steps

High availability means tackling: fault-tolerance, scalability, distribution, responsiveness, live-update.

The basic concurrency primitive is called an Erlang process and typical Erlang systems run thousands or millions of these.
BEAM (the erlang virtual machine) uses its own schedulers to distribute the processes over the available CPU cores, parallelizing execution.

Fault tolerance - Erlang processes are completely isolated from each other as Erlang processes share no memory.
Scalability - Erlang processes communicate via asynchronous messages, each process is efficiently parallelized and can take advantage of all CPU cores.
Distribution - Communication between processes works the same way regardless of if processes reside in the same BEAM, so ready to be distributed over multiple machines.
Responsiveness - Erlang handles the execution of multiple process by employing dedicated schedulers that interchangeably execute many processes.
                 A scheduler is preemptive - it gives a small execution window to each process, then pauses it and runs another process.
                 As the execution window is small, a single long-running process can't block the rest of the system.

Erlang is more than a programming language, it's a full-blown development platform with 4 distinct parts: the language, the virtual machine, the framework and the tools.

The language is the primary way of writing code that runs in the Erlang virtual machine. It's a simple functional language with basic concurrency primitives.
This is compiled to bytecode which is executed in the BEAM.
BEAM parallelizes the concurrent Erlang programs and takes care of process isolation, distribution and system responsiveness.

The OTP framework (which ships with Erlang) abstracts away many typical Erlang tasks:
+ Concurrency and distribution patterns
+ Error detection and recovery in concurrent systems 
+ Packaging code into libraries
+ Systems deployment
+ Live code updates

OPT has tools to: compile Erlang code, start a BEAM instance, create deployable releases, run the interactive shell, connect to the running BEAM instance.

Elixir is an alternative language for the Erlang virtual machine that allows you to write cleaner code.
Elixir source code compiles to BEAM-compliant bytecode that can run in a BEAM instance.
Elixir can cooperate with pure Erlang code and visa versa and Elixir libraries are as performant as their Erlang counterpart.

Erlang has shortcomings in both speed and ecosystem.
Erlang wasn't build for speed, but for reliability and consistency.

## 2 Building blocks

Elixir data can't be mutated. The result of modifying input data resides in another memory location.
New variables created by referencing another variable will contain a shallow copy of the initial data.
When you rebind a variable, the variable references another memory location and the old location isn't accessible and is available for garbage collection.

When modifying the nth element of a list, the new version will contain shallow copies of the first n-1 elements, followed by the modified element.
So adding elements to the end of a list is expensive - to append a new element at the tail, you have to iterate and (shallow) copy the entire list.

By contrast, pushing an element to the top of a list doesn't copy anything, which makes it the least expensive operation.

Two benefits of immutability: side-effect-free functions (easier to analyze and test) and data consistency.

## 3 Control flow

We should use `Stream.with_index/1` to save iterating through the list twice if we don't need the index right away.
Look to use the `Stream` module instead of `Enum` when we don't need the data right away.

## 4 Data abstractions

Check out `Kernel.put_in` and `Kernel.get_and_update_in` docs (https://hexdocs.pm/elixir/1.0.5/Kernel.html) - mental stuff.

Polymorphism is a runtime decision about which code to execute based on the input data. In Elixir, the basic way of doing this is through protocols.

Protocols allow us to extend the original behaviour for as many data types as we need.

## 5 Concurrency primitives

A highly available system is one that runs forever and are always able to meaningfully respond to client requests.

+ Fault-tolerance — Minimize, isolate, and recover from the effects of runtime errors.
+ Scalability — Handle a load increase by adding more hardware resources without changing or redeploying the code.
+ Distribution — Run your system on multiple machines so that others can take over if one machine crashes.

In BEAM, the unit of concurrency is a process: a basic building block that makes it possible to build scalable, fault-tolerant, distributed systems.

`spawn/1` takes a function which it will execute in another process.

See: https://elixir-lang.org/getting-started/processes.html

Let’s try message-passing with the concurrent queries developed in the previous section.
In your initial attempt, you’ll run queries in separate processes and print them to the screen from those processes.
Then you’ll try to collect all the results in the main process.

```iex
pid = self()
Enum.map(1..5, fn i -> spawn(fn -> Process.sleep(2000);IO.puts(i);send(pid, i) end) end)
Enum.map(1..5, fn _ -> receive do; message -> "message: #{message}";after 1000 -> "no more messages";end end)
receive do; message -> "message: #{message}";after 1000 -> "no more messages";end
```

A server process can be a process which runs forever, and for this we use endless tail recursion.
In Elixir, if the last thing a function does is call another function (or itself),
a simple jump takes place instead of a stack push.
Consequently, a function that always calls itself will run forever,
without causing a stack overflow or consuming additional memory.

```bash
iex -S mix
iex(1)> DatabaseServer.start()
...
```

Processes share no memory, so sending a message to another process results in a deep copy of the message contents.
This should be reasonably fast, but having many processes, frequently sending big messages may affect system performance.
A list of a million complex structures is big.
Deep copying doesn't take place with some binaries.

BEAM uses as many schedulers as there are logical processors available.
There are, in general, n schedulers that run m processes, where m >> n.
You can run a large number of logical microthreads using a smaller number of OS threads.

# 6 Generic server processes

An OTP convention is:
Synchronous requests to server processes are called a 'call'.
Asynchronous, fire-and-forget requests to server processes are called a 'cast'.

Elixir ships with a generic server process, called `GenServer`. It handles edge cases and has features:
+ Support for calls and casts.
+ Customizable timeouts for call requests.
+ Propagation of server-process crashes to client processes waiting for a response.
+ Support for distributed systems.

A behaviour is generic code that implements a common pattern.

# 7 Building a concurrent system

For our todo list example, we will run one instance of the todo server for each todo list.
Each list is managed concurrently and the system should be more responsive and scalable.
We will map todo list names to todo server pids.
We have a todo cache, which we will run only one instance of this process.

```iex
# useful for getting the number of currently running processes
:erlang.system_info(:process_count)
```

Note: we should be aware of the bottleneck of having 1 Todo.Cache process
as this will handle every todo list.

Addressing the process bottleneck can be done by: bypassing the process, handling the requests concurrently and limiting concurrency with pooling.

There are various reasons for running code in a dedicated server process:
+ the code must manage a long-living state
+ the code handles a kind of resource that can and should be reused, such as a TCP or database connection, file handle, pipe to an OS process etc.
+ Only one process may run this code in any moment.
if none of these conditions are met, you probably don't need a separate process and can run the code in the client process.

If you handle the requests concurrently, this has the same drawbacks as concurrency is unbounded, so too many simultaneous clients might overload the disk I/O.

A typical remedy for this problem is pooling. For example, you database process might create 3 worker processes and keep their pids in its internal state.
When a request arrives, it's delegated to one of the worker processes.

Task: change `Todo.Database` to use a pool of processes (pg 199)

# 8 Fault-tolerance basics

When a process crashes, we usually want to detect this state and do something about it.

When a runtime error happens, execution control is transferred up the call stack to the error-handling code.

Compared to c++, c#, java and javascript, there's much less need to catch runtime errors - we instead adopt the 'let it crash' mindset.

`spawn_link` can be used instead of `spawn` to ensure that if one process errors, the other one will be connected and also crash.
A child `spawn_link`ed process will emit an exit signal to it's parent process.

Usually we don't want a linked process to crash. Instead, we want to detect the process crash and react. This can be done by "trapping exits".

```iex
iex(1)> spawn(fn ->
  Process.flag(:trap_exit, true)

  spawn_link(fn -> raise("Something went wrong") end)

  receive do
    msg -> IO.inspect(msg)
  end
end)
```

We can also make use of `monitor_ref = Process.monitor(target_pid)` for the case when process A is notified when B terminates, but not the other way around.

A supervisor is a generic process that manages the lifecycle of other processes in a system. It can start and restart child processes.

Processes that aren't supervisors are called "workers".

# 9 Isolating error effects

Always make sure `init/1` runs quickly. We can `send` a message to make it asynchronous.

Restart strategies:
+ `:one_for_one` - a supervisor handles a process termination by starting a new process in its place, leaving other children alone.
+ `:one_for_all` - when one child crashes, all children are restarted (useful when there's a tight dependency in all directions - every sibling depends on other siblings).
+ `:rest_for_one` - when a child crashes, the supervisor restarts all younger siblings of the crashed child (useful if younger siblings depend on older ones).

Using `Supervisors` instead of `GenServer` can be used to localize the impact of an error.

The Elixir standard library includes a `Registry` module for process registry, it allows you to associate a process with a key.
This key can be subsequently looked up.
`Registry` links to all registered processes, and terminated processes are automatically removed.
This means that we can easily reference a particular process, even if the `pid` changes. So useful if processes are restarted.

Elixir comes with a `DynamicSupervisor` which is useful for starting child processes on demand.

# 10 Beyond GenServer

Processes started with a plain `spawn` or `spawn_link` are not OPT-compliant - as in it follows the OTP design principles orientated around processes.
Instead we can use the `Task` module.

For awaited `Task`s, we can use `Task.async/1` and `Task.await/1`.
For non-awaited `Task`s, we can use `Task.start_link/1`.

If we only implement `init/1`, `handle_cast/2` and `handle_call/3`, we can replace the `GenServer` with `Agent`.
If we need `handle_info/2` or `terminate/1`, then `Agent` won't suffice.

One idea is to always just use `GenServer` though as converting from `Agent` to `GenServer` requires work and always using `GenServer` keeps code more uniform.

ETS tables are useful if you want to share system-wide state without introducing a dedicated server.

# 11 Working with components

An OTP application is a component that consists of multiple modules and that can depend on other applications.

If we have a `def application` in our mix file, created when running `mix new --sup ...`, then the application will start when you run `iex -S mix`.
The critical part of our `mix.exs` file here is `mod: {App, []}`, specified by `application/0`.
When you start the application, the function `App.Applicatoin.start/2` is called with `[]`.

It is useful after running an application to run `iex -S mix`, then `:observer.start()`.
The Applications tab will give a system overview.

# 12 Building a distributed system

BEAM-powered distributed systems are built by connecting multiple nodes into a cluster. A node is a BEAM instance that has a name associated with it.

It can be started with (where `sname` is short name):

```bash
iex --sname node1@localhost
> node()
:node1@localhost
```

Cluster-wide locks are implemented in the `:global` module. They can be useful if you need to ensure a large amount of data is serialized in the entire cluster.
In this very rare case, the data is large when sent over a network, but we can get all nodes to synchronize and pass a smaller message instead.
`:global.trans/2` ensures cluster-wide isolation.

The cluster we've created works, but there are remaining issues to address which aren't addressed in ch12:
+ Load balancer for single access point for all clients.
+ New node data synchronization.
+ Two phase commit database strategy.
+ Handling of network partitions.

# 13 Running the system

Running the system amounts to the following:
1. Compile all modules into `.beam` files.
2. Start the BEAM instance and set up load paths to include all locations from step 1.
3. Start all required OTP applications.

This all happens when we run `iex -S mix`. In production, we would instead want to run: `mix run --no-halt` - which won't start the interactive shell.

To start a detached system (for production), we run:

```bash
MIX_ENV=prod elixir --erl "-detached" --sname todo_system@localhost -S mix run --no-halt
# we can check this is running with:
epmd -names
# we can connect to a running BEAM instance with:
iex --sname debugger@localhost --remsh todo_system@localhost --hidden
```

#### OTP releases

An OTP release is a stand-alone, compiled, runnable system that consists of the minimum set of OTP applications needed by the system.
A release doesn't contain artefacts, such as source code, documentation files or tests.
This allows us to build the system on our development machine and ship only the binary artefacts.
This means that the host machine doesn't need to have any tools installed - including needing Erlang or Elixir.

We use distillery for releases here, instructions are in `todo/README.md`.

Analysing system behaviour would take a book, a recommended great place to start is: Stuff Goes Bad: Erlang in Anger the free eBook (http://www.erlang-in-anger.com).

Erlang ships with a GUI-based debugger (http://erlang.org/doc/apps/debugger/debugger_chapter.html) that can be invoked with:

```iex
:debugger.start()
```

We can also use the observer GUI application and connect to a remote node.
The production system doesn't need to run the observer application, but it needs to contain modules that gather data for the remote observer application.

This can be done in our `mix.exs` file:

```elixir
...

def application do
  [
    extra_applications: [:logger, :runtime_tools]
  ]
end
...
```

Debugging highly concurrent systems are difficult as breakpoints are less feasible.

We instead should opt for logging and tracing. When something goes wrong, we want as much information as possible.

A debugging overview can be found here: https://elixir-lang.org/getting-started/debugging.html.

We should also consider benchmarking and profiling tools to aid in debugging.

In production, we shouldn't rely on `IO.inspect`, instead we should rely on a logger application, see: https://hexdocs.pm/logger/Logger.html.

To turn on tracing do the following:

In one window start the server:

```bash
_build/dev/rel/todo/bin/todo start
```

Connect to the instance in another window:

```bash
_build/dev/rel/todo/bin/todo remote
```

When we hit a todo list we initially don't see any trace:

```bash
curl "http://localhost:5454/entries?list=bob&date=2018-12-19"
```

But in the `remote` window, if we run:

```bash
:sys.trace(Todo.Cache.server_process("bob"), true)
```

We see tracing whenever we touch that todo list.

We can turn off tracing by setting:

```bash
:sys.trace(Todo.Cache.server_process("bob"), false)
```

13.3 Analysing system behaviour (pg 371).
(Finish pg 376).


