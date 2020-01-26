/**
 *
 * utils.c - misc utility functions
 *
 **/


#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

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
        internal_error("is_number called on null string");
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

// Concatenates the source string onto the end of the buffer which is
// already initailized as a null-terminated string. Count is the maximum
// number of characters to include (excluding the null)
void str_concat(char *buf, const char *source, size_t count) {
    if (source != NULL && buf != NULL) {
        int current_len = strlen(buf);
        strncat(buf + current_len, source, count - current_len);
    }
}

// Are both arguments strings that are equal
bool str_equals(mal a, mal b) {
    return a.tag == STR &&
        b.tag == STR &&
        strcmp(a.s, b.s) == 0;
}

// Remove escape sequences from a string
mal remove_escapes(mal m) {
    if (!is_str(m)) {
        return mal_exception(mal_str("remove_escapes on a non-string"));
    }
    char *buf = checked_malloc(strlen(m.s) + 1, "remove_escapes");
    bool in_escape = false;
    char *p = buf;
    const char *s = m.s;
    while (*s) {
        if (in_escape) {
            if (*s == 'n') {
                *p++ = '\n';
                s++;
                in_escape = false;
            } else if (*s == '\\') {
                *p++ = '\\';
                s++;
                in_escape = false;
            } else if (*s == '\"') {
                *p++ = '\"';
                s++;
                in_escape = false;
            } else {
                return mal_exception(mal_str("Bad escape sequence"));
            }
        } else {
            if (*s == '\\') {
                in_escape = true;
                s++;
            } else {
                *p++ = *s++;
            }
        }
    }
    if (in_escape) { // trailing slashj or bad escape
        return mal_exception(mal_str("EOF in escape sequence"));
    }
    *p = *s; // null terminate;
    return mal_str(buf);
}

// add escape sequences to a string
mal add_escapes(mal m) {
    if (!is_str(m)) {
        return mal_exception(mal_str("add_escapes on a non-string"));
    }

    // need to make one extra char space for every ", \, \n
    const char *s = m.s;
    int extras_count = 0;
    while (*s) {
        if (*s == '\\' || *s == '\"' || *s == '\n') {
            extras_count++;
        }
        s++;
    }
    char *buf = checked_malloc(strlen(m.s) + 1 + extras_count, "add_escapes");

    char *p = buf;
    s = m.s;
    while (*s) {
        if (*s == '\\') {
            *p++ = '\\';
            *p++ = '\\';
            s++;
        } else if (*s == '\"') {
            *p++ = '\\';
            *p++ = '\"';
            s++;
        } else if (*s == '\n') {
            *p++ = '\\';
            *p++ = 'n';
            s++;
        } else {
            *p++ = *s++;
        }
    }
    *p = *s; // null terminate;
    return mal_str(buf);
}
