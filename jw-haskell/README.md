# jw-haskell

My Haskell implementation of make-a-list

Design choices:

* Text used internally throughout
* megaparsec
*


Tools used

* hlint
* brittany
  + install with: stack install brittany
  + run with: brittany --write-mode=inplace app/*.hs src/*.hs


TODO

Phase 1:
How to handle an empty input (or a comment only input)

Phase 1 deferables:
* String type with escapes
* Error checking to match parens
* Reader macros
* Keyword, vector, hash-map