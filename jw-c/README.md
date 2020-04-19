Todo

- Fix intermittent
- in step 9
(try* abc (catch* exc (prn "exc is:" exc))) gives "exc is:\xe0"
- Intermittent failures in steps 6, A

- Add nyi functions - seq and conj

- Add asserts more thoroughlly?
- Check exception propogation - done in core_atom


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
