#ifndef READER_H
#define READER_H

#include "types.h"

// Read a mal ast from a string. May return a missing mal value which should be
// caught before passing the result to print or eval
mal read_str(const char *);

#endif
