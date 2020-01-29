Todo
  - Tidy
  - Tests for core
  - More tests
  - debug levels? 1 for mal level, 2 for internal. Better util for mal errors
  - function to show env
  - do we need the internal_errors - use assert instead, combined mal_exception/str, no pretty print for exception

using only libs builtin on mac
readline
pcre
using C11 (anonymous unions/structs)


Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?
