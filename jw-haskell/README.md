# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Text used internally throughout
* megaparsec (using its lex helpers)


Tools used

* stack
  + use stack test --fast
* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

Phase 1:
* Fix magicPrefix
* Printer for keywords
* Implement @
* Vector and map underlying rep (mutable? what to do with odd-length map literals?)
