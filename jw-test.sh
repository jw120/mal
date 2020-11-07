#!/bin/sh

# fastest to slowest
impls="jw-haskell jw-c jw-racket jw_elixir jw-swift jw-python"

for impl in $impls
do
  echo $impl
  make "test^$impl" | grep "failing\|TEST RESULTS" | grep -v "   0:"
done

