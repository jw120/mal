Todo

- Tidy code (done through core*.*)
- Function docs in header files only (and in .c for static files)
- Review code and tidy
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
