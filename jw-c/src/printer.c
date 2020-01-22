#include <stdio.h>

#include "printer.h"
#include "utils.h"

#define BUFFER_SIZE_FOR_INT 16

// return a string representation of the mal value
const char *pr_str(mal m)
{
    int chars_needed;
    char *buf;

    switch (m.tag) {
        case INT:
            chars_needed = snprintf(NULL, 0, "%d", m.i);
            buf = checked_malloc(chars_needed + 1, "pr_str INT");
            snprintf(buf, chars_needed, "%d", m.i);
            return buf;
        case STR:
            chars_needed = snprintf(NULL, 0, "\"%s\"", m.s);
            buf = checked_malloc(chars_needed + 1, "pr_str STR");
            snprintf(buf, chars_needed, "\"%s\"", m.s);
            return buf;
        case SYM:
            return m.s;
        case LIST:
//            list_node *elems = map_pr_str(m.n);
            return "<list>";
    }
}

