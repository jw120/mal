Next for step1
- String reading - do escaping and catch unbalanced quotes/escapes
- Add special chars ` etc
- Add hashmaps

Sometime
- More tests for seq and types?
- All free's for all mallocs?
- Eliminate internal_errors?

using only libs builtin on mac
readline
pcre
using C11 (anonymous unions/structs)



Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?
