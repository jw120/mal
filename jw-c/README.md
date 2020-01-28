Next for step1
- Get map to work
    - Support symbols
    - Deduplicate keys
    - Fix map equality (and add tests for map_eq and seq_eq)
- Review and tidy
    - do we need the internal_errors
    - debug levels?
    - more tests?
- Add proper hashmap

using only libs builtin on mac
readline
pcre
using C11 (anonymous unions/structs)


Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?
