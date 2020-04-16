#ifndef PRINTER_H
#define PRINTER_H

#include "types.h"

// Return a string representation of the mal value. If the print_readably is
// true, then newlines, slashes and double quotes are escaped with slashes when
// part of a mal string
const char *pr_str(mal, bool print_readably);

#endif
