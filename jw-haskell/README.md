# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Megaparsec (using its lex helpers) for parsing
* Text used internally throughout
* Application monad using Either for errors, State for environment along with IO

Tools used

* stack
  + use stack test --fast or stack test --fast --file-watch
* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

* DECIDE ON TESTING STRATEGY
* Phase 1 - better errors for parsing
* Phase 1 - vector underlying rep
* Check our magic keyword string is what we think it is
