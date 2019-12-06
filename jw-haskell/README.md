# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Text used internally throughout
* megaparsec (using its lex helpers)
* Either for parsing errors
* ?? for eval errors


Tools used

* stack
  + use stack test --fast or stack test --fast --file-watch
* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

* Map parsing
* Tidy
* Improve Makefile and exec
* AST is not Lits
* Phase 1 - better errors for parsing
* Phase 1 - vector and map underlying rep (mutable? what to do with odd-length map literals?)
* Check our magic keyword string is what we think it is
