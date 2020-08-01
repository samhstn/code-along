# Learn you a haskell for great good

To get up and running with the repl, do the following:

```bash
# install the `stack` command
# see here: haskellstack.org
brew install haskell-stack

# start the ghci repl
# this will start a long download
stack ghci

# we can always set the same prompt by running:
Prelude>:set prompt "ghci> "
```

To load a `hs` script called `index.hs` in the interpretor, we run:

```
ghci> :l index
```

Note: when using the `++` operator on long strings, Haskell has to walk through the whole list on the left.
Using the `:` (cons) operator is instantaneous.

```
ghci> 'A':" SMALL CAT"
"A SMALL CAT"
ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```

`[1,2,3]` is actually syntactic sugar for `1:2:3:[]`.

`!!` gets an element from a list by index.

```
ghci> "Steve Buscemi" !! 6
'B'
```

`Int` is for integers between 2147483647 and -2147483647 (2^32).

`Integer` is for much bigger integers, but not as efficient.

`Float` is a real floating point with single precision.

`Double` is a real floating point with double precision.
