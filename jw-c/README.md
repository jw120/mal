Todo

- Tidy code (done upto reader.c)
- Add tests for str_join in utils_test
- Add asserts more thorogulhly

- Function docs in header files only (and in .c for static files)

- Check have exception short circurting everywhere (as in equals)

readline
pcre
using C11 (anonymous unions/structs)
more tests than other versions as c is flaky..

core_ used to name (static) function in core
mal_ for functions related to the mal type

Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?

Comments for extern functions in .h, static functions in .c
