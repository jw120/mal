# jw-haskell

My Haskell implementation of make-a-list

How to run tests (all run from parent directory, `mal`)

```
make "test^jw-haskell^step0"  # Run one step of the tests (step runs from 0..A)
make "test^jw-haskell"  # Run all the tests
make MAL_IMPL=jw-haskell "test^mal"  # Run all the tests in self-hosting mode
```

Design choices:

* Megaparsec (using its lex helpers) for parsing
* Text used internally throughout
* Application monad using Either for errors and IO to allow for
  + Printing
  + Modifications to environments (IORefs)
  + Atoms (IORefs)
 * Minimal tests (just for Reader) as rely on github provided test suite in mal

Tools used

* stack
  + use stack test --fast or stack test --fast --file-watch
* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

* Shoudl we ever be preserving meta? (in Core.hs)
* haddocks
* should equality look at meta?
* do we need time package in package.yaml?
* work out how to format code more nicely
* Phase 1 - better errors for parsing
* Phase 1 - vector underlying rep
* Check our magic keyword string is what we think it is
