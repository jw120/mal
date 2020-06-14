#ifndef ESCAPES_H
#define ESCAPES_H

#include "types.h"

// Return a newly (malloc'ed) C string that is a copy of the input string, but
// with newline, backslash and double-quote replaced by \n, \\ and \".
const char *add_escapes(const char *);

// Return a mal string that includes a newly (malloc'ed) C string that is a copy
// of the input string, but with \n, \\ and \" replaced by newline, backslash
// and double-quote characters. Returns an exception with malformed escape
// sequences
mal remove_escapes(const char *);

#endif
