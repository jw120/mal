Todo
  - combined mal_exception/str
  - New logic for debug
  - Tidy - tests for all, comments for all - done core and env
  - debug levels? 1 for mal level, 2 for internal. Better util for mal errors
  - function to show env
  -  no pretty print for exception

readline
pcre
using C11 (anonymous unions/structs)
more tests than other versions as c is flaky..

Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?
