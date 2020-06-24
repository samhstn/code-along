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

We can't put user defined functions in guards, this is because we want pattern matching to always terminate and to be computed quickly. This is achieved through only allowing simple calculations.
