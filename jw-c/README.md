Todo

- Intermittent failures in steps 6 and 9
- Steps 8 and beyond fail in self-hosting

- Add nyi functions

- Add asserts more thoroughlly?
- Check exception propogation - done in core_atom
- check use of strncat srncpy. (copying null).


- Check have exception short circurting everywhere (as in equals)

readline
pcre
using C11 (anonymous unions/structs)
more tests than other versions as c is flaky..
checking for exceptions manually
Discriminated union


- Function docs in header files only (and in .c for static files)
core_ used to name (static) function in core
mal_ for functions related to the mal type

Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?

Comments for extern functions in .h, static functions in .c
