#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

bool debug_mode = true;

#define INTERNAL_ERROR_PREFIX "Internal error: "

noreturn void internal_error(const char * restrict fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fflush(stdout);
    fprintf(stderr, INTERNAL_ERROR_PREFIX);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
}

void debug(const char *func, const char * restrict fmt, ...) {
    if (debug_mode) {
        va_list args;
        va_start(args, fmt);
        printf("%16s: ", func);
        vprintf(fmt, args);
        printf("\n");
    }
}

void *checked_malloc(size_t size, const char * restrict fmt, ...) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        fprintf(stderr, "Malloc failed - ");
        va_list args;
        va_start(args, fmt);
        internal_error(fmt, args);
    }
    return ptr;
}

bool is_number(const char *s) {

    debug("is_number", "called with %s", s);

    if (s == NULL) {
        internal_error("is_number", "null string");
    }
    if (strlen(s) == 0) {
        return false;
    }
    if (s[0] == '-') {
        return is_number(s + 1);
    }
    for (const char *p = s; *p; p++) {
        if (!isdigit(*p)) {
            return false;
        }
    }
    debug("is_number", "succeeded");
    return true;
}