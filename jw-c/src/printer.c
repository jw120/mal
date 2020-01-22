#include <stdio.h>

#include "printer.h"
#include "utils.h"

#define BUFFER_SIZE_FOR_INT 16

// return a string representation of the mal value
const char *pr_str(mal m)
{
    int buf_size;
    char *buf;

    switch (m.tag) {
        case INT:
            debug("pr_str", "int %d", m.i);
            buf_size = 1 + snprintf(NULL, 0, "%d", m.i);
            buf = checked_malloc(buf_size, "pr_str INT");
            snprintf(buf, buf_size, "%d", m.i);
            return buf;
        case STR:
            debug("pr_str", "str \"%s\"", m.s);
            buf_size = 1 + snprintf(NULL, 0, "\"%s\"", m.s);
            buf = checked_malloc(buf_size, "pr_str STR");
            snprintf(buf, buf_size, "\"%s\"", m.s);
            return buf;
        case SYM:
            debug("pr_str", "sym %s", m.s);
            return m.s;
        case LIST:
            debug("pr_str", "list");

//            list_node *elems = map_pr_str(m.n);
            return "<list>";
    }
}

