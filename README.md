# CORE
Implementation of SPJ's Tutorial Core compilers written using modern Haskell
programming practices (mostly Monadic error handling and parsing using Parsec)

The template instantiation machine was partially implemented, the GMachine is
entirely implemented (up to Mk6).

For now, load Main into GHCi and use runCore and specify a compiler to use.
This interface will soon be better organized as a Haskeline REPL.
```haskell
>>> runCore gmachineMk5 "main = fac 20 ; fac n = if (n <= 0) 1 (n * (fac (n - 1)))"
"Right (NNum 2432902008176640000)"

>>> runCore gmachineMk6 "main = fac 20 ; fac n = if (n <= 0) 1 (n * (fac (n - 1)))"
"2432902008176640000 "
```

I'll be working through the 3 other compilers in the tutorial. I want to learn
how Haskell works, and this tutorial seems like a good rock-bottom starting
point. I'll be done once I implement a [STG machine](http://research.microsoft.com/apps/pubs/default.aspx?id=67083), a more updated version of which is used in the current Haskell compilation process.



Also, I mainly used NixOS for this, sandboxed installation is easy with
```
nix-shell core.nix
```

Referencing [Implementing functional languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/Papers/pj-lester-book/i).
