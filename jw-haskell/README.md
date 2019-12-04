# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Text used internally throughout
* megaparsec (using its lex helpers)


Tools used

* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs
* hspec for tests

TODO

Phase 1 deferables:
* Error checking to match parens
* Reader macros
* Keyword, vector, hash-map