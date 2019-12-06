# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Text used internally throughout
* megaparsec (using its lex helpers)
* Either for parsing errors and for eval errors

*** How do we make persistent? Either have a mutable state or force tests to run in a chain?

Tools used

* stack
  + use stack test --fast or stack test --fast --file-watch
* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

* Improve Makefile and exec
* Phase 1 - better errors for parsing
* Phase 1 - vector underlying rep
* Check our magic keyword string is what we think it is
