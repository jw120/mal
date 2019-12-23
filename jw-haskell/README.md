# jw-haskell

My Haskell implementation of make-a-list

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

* Change types to put meta at the front so (\xs -> ASTVector xs meta) can become (ASTVector meta)
* haddocks
* should equality look at meta?
* do we need time package in package.yaml?
* work out how to format code more nicely
* Phase 1 - better errors for parsing
* Phase 1 - vector underlying rep
* Check our magic keyword string is what we think it is
