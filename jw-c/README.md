Todo

Next
* Get tests to pass again
* Re-factor ttests with mal-specific helper functions in minunit


readline
pcre
using C11 (anonymous unions/structs)
more tests than other versions as c is flaky..

Better map

Re-factor core_is

Better tests



core_ used to name (static) function in core
mal_ for functions related to the mal type

Think about how to manage memory
- mal atoms all on heap? collections with malloc?
- do we free strings? on read?
- do we ever garbage collect?
