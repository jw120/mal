/**
 *
 * types.c - basic support for the mal type defined in mal.h
 *
 **/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "types.h"
#include "seq.h"
#include "utils.h"

// Value equality
bool mal_equals(mal a, mal b) {
    switch (a.tag) {
        case INT:
            return a.tag == b.tag && a.i == b.i;
        case SYM:
        case STR:
            return a.tag == b.tag && strcmp(a.s, b.s) == 0;
        case LIST:
        case VEC:
            if (b.tag == LIST || b.tag == VEC) {
                return seq_equals(a, b);
            }
            return false;
        default:
            internal_error("Unknown tag in mal_equals", a.tag);
    }
}

/**
 *
 * Convenience functions to test the type
 *
 */

bool is_missing(mal m) {
    return m.tag == MISSING;
}

bool is_exception(mal m) {
    return m.tag == EXCEPTION;
}

bool is_int(mal m) {
    return m.tag == INT;
}

bool is_str(mal m) {
    return m.tag == STR;
}

bool is_sym(mal m) {
    return m.tag == SYM;
}

bool is_list(mal m) {
    return m.tag == LIST;
}

bool is_vec(mal m) {
    return m.tag == VEC;
}

bool is_seq(mal m) {
    return m.tag == LIST || m.tag == VEC;
}

bool match_sym(mal m, const char * s) {
    return m.tag == SYM && strcmp(m.s, s) == 0;
}


/**
 * Convenience constructor functions
 *
 */

mal make_missing() {
    mal val = { MISSING };
    return val;
}

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

mal make_vec(vec *v) {
    mal val = { VEC, { .v = v }};
    return val;
}

mal make_exception(mal m) {
    mal *m_ptr = checked_malloc(sizeof(mal), "make_exception");
    *m_ptr = m;
    mal val = { EXCEPTION, { .e = m_ptr }};
    return val;
}

// Constants to simplify evaluation
mal opening_paren = { SYM, { .s = "("} };
mal closing_paren = { SYM, { .s = ")"} };
