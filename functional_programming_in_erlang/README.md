# Functional Programming in Erlang

Erlang command line interface can be started with:

```bash
$ erl
> % Every line must end with a '.'
> A = [1, 2, 3]
> % Bound variables can be forgotten with
> f(A).
> % or to forget all bound variables
> f().
> % to compile a file `filename.erl` run
> c(filename).
> % to quit the shell type
> q().
> %
```

When writing the factorial function, we think about how to handle the negative case. We should opt for a `when` test for N>0. Now when we use a negative number, we get a failure of pattern matching, this is an example of the Erlang philosophy of "let it fail". We don't know what we should do in the N<0 case, so we let it fail and let the caller handle.

Q: Which of the following statements about the time complexity of calling foo(Xs) is true?

```erlang
foo([X|Xs]) -> bar(foo(Xs),[X]);
foo([])     -> [].

bar([],Ys) -> Ys;
bar([Z|Zs],Ys) -> [Z|bar(Zs,Ys)].
```

A: 'It is quadratic in the length of Xs.'
Not: 'It is logarithmic in the length of Xs'.
Not: 'It is linear in the length of Xs.'
Not: 'It is exponential in the length of Xs.'

Explanation: bar is linear in its first argument, but it is called recursively for each N length less than the length of Xs, hence the complexity is quadratic.

We can't put user defined functions in guards, this is because we want pattern matching to always terminate and to be computed quickly. This is achieved through only allowing simple calculations.

Erlang is weekly typed, but we can use Typer, which is built on top of Dyalyzer, to help us.

Dyalyzer takes quite a long time to run, we can run it in an Erlang project with:

```bash
dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl eunit
```

This takes quite a long time, but once it's done once, we don't need to do it again.

We can then run:

```bash
typer list.erl
typer recursion.erl
```

This will tell us the types of our functions, and if our current types are correct.
