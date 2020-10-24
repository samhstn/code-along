# Chapter 1 - Unit Tests

A problem well-stated is half-solved.

A test typically has 4 stages:

+ setup (optional) - preparing data to pass as a parameters to the code under test or staging data in some shared state.
+ exercise - the call the run the code under test.
+ verify - make assertions about the behavior of your code (sometimes inline with the exercise step).
+ teardown (optional) - return shared state to how it was before the setup phase.

If our test suite is written well, it serves as a secondary source of documentation.

The `on_exit` function is useful for controlling shared state in our `setup` block.

dependency - any code that your code relies on.
dependency injection - any system that allows your code to utilize a dependency without hard-coding the name of the dependency,
                       allowing any unit of code that meets a contract to be used.

In Elixir, we an inject a dependency as a parameter to a function and through the application environment.
