# CORE
SPJ's Tutorial Core compilers written using circa 2015 Haskell
programming practices. Big changes include Monadic error handling and parsing
using Parsec.

The template instantiation machine was partially implemented and the GMachine is
entirely implemented (up to Mk6).

Simply run Main.hs. You can change the compiler used in this simple REPL,
though it doesn't have multiline editing or a nice file-loading interface (yet).
```
runhaskell Main.hs
core >>> main = fac 20 ; fac n = if (n <= 0) 1 (n * (fac (n - 1)))
2432902008176640000

core >>> .compiler gmachinemk1
changing compiler to gmachinemk1

core >>> main = 4 * 10
undeclared global: *

core >>> .compiler gmachinemk4
changing compiler to gmachinemk4

core >>> main = 4 * 10
NNum 40
```

Can also run through GHCi
```haskell
ghci Main.hs
>>> runCore gmachineMk5
"NNum 2432902008176640000"

>>> runCore gmachineMk6 "main = fac 20 ; fac n = if (n <= 0) 1 (n * (fac (n - 1)))"
"2432902008176640000 "
```

<!--
UPDATE: Didn't happen. Things have been going well though.

I'll be working through the 3 other compilers in the tutorial. I want to learn
how Haskell works, and this tutorial seems like a good rock-bottom starting
point. I'll be done once I implement a [STG machine](http://research.microsoft.com/apps/pubs/default.aspx?id=67083), a more updated version of which is used in the current Haskell compilation process.
-->



Also, I mainly used NixOS for this (Cabal file broken, atm). Sandboxed installation is easy with
```
nix-shell shell.nix
```

Referencing [Implementing functional languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/Papers/pj-lester-book/i).
