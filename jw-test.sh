#!/bin/sh

# fastest to slowest
impls="jw-haskell jw-c jw-racket jw_elixir jw-swift jw-python"

for impl in $impls
do
  echo "$impl"
  if [ "$impl" = "jw-racket" ]
  then
    echo "Expect 14 soft failures on step A as meta not implemented (same for hosted)"
  fi
  make "test^$impl" | grep "failing\|TEST RESULTS" | grep -v "   0:"
  echo "mal hosted on $impl"
  make MAL_IMPL="$impl" test^mal | grep "failing\|TEST RESULTS" | grep -v "   0:"
done

