#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define INTERNAL_ERROR_PREFIX "Internal error: "
#define INTERNAL_ERROR_SUFFIX "\n"

noreturn void internal_error(const char * restrict fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf(INTERNAL_ERROR_PREFIX);
    vprintf(fmt, args);
    printf(INTERNAL_ERROR_PREFIX);
    fflush(stdout);
    exit(EXIT_FAILURE);
}