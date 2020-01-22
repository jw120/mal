#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "list.h"
#include "utils.h"

// Value equality
bool mal_equals(mal a, mal b) {
    if (a.tag != b.tag) {
        return false;
    }
    switch (a.tag) {
        case INT:
            return a.i == b.i;
        case SYM:
        case STR:
            return strcmp(a.s, b.s) == 0;
        case LIST:
            return list_equals(a.n, b.n);
        default:
            internal_error("Unknown tag in mal_equals", a.tag);
    }
}

// Convenience constructor functions

mal make_int(int i) {
    mal val = { INT, { .i = i } };
    return val;
}

mal make_str(const char *s) {
    mal val = { STR, { .s = s } };
    return val;
}

mal make_sym(const char *s) {
    mal val = { SYM, { .s = s } };
    return val;
}

mal make_list(list_node *n) {
    mal val = { LIST, { .n = (list_node *) n } };
    return val;
}

// Constants to simplify evaluation
mal opening_paren = { SYM, { .s = "("} };
mal closing_paren = { SYM, { .s = ")"} };
