/**
 *
 * printer.c - provides the pr_str to convert a mal type to a character string
 *
 **/


#include <stdio.h>
#include <string.h>

#include "printer.h"
#include "seq.h"
#include "utils.h"

#define BUFFER_SIZE_FOR_INT 16

// given a list of strings, concat with spaces and surround with (..) or [..]
static const char *join_strings(list_node *s, int chars, int elements, bool is_vector) {

    const char *opener = is_vector ? "[" : "(";
    const char *closer = is_vector ? "]" : ")";

    int num_spaces = elements > 0 ? elements - 1 : 0;
    int buf_size = 1 + 2 + chars + num_spaces;
    char *buf = checked_malloc(buf_size, "join_string");

    str_concat(buf, opener, buf_size - 1);
    while (s != NULL) {
        str_concat(buf, s->val.s, buf_size - 1);
        if (s->next != NULL) {
            str_concat(buf, " ", buf_size - 1);
        }
        s = s->next;
    }
    str_concat(buf, closer, buf_size - 1);
    return buf;
}

static const char *print_list(list_node *input_head) {
    debug("pr_str", "print_list %p", input_head);
    list_node *string_head = NULL;
    int char_count = 0;
    int element_count = 0;
    list_node * input_node = input_head;
    list_node * string_node = string_head;
    while (input_node != NULL) {
        const char * s = pr_str(input_node->val);
        char_count += strlen(s);
        element_count++;
        string_node = list_extend(mal_str(s), string_node);
        if (string_head == NULL) {
            string_head = string_node;
        }
        input_node = input_node->next;
    }
    return join_strings(string_head, char_count, element_count, false);
}

static const char *print_vec(vec *v) {
    debug("pr_str", "print_vec %p", v);
    if (v == NULL) {
        return "[]";
    }
    list_node *string_head = NULL;
    int char_count = 0;
    int element_count = 0;
    list_node *string_node = string_head;
    for (int i = 0; i < v->size ; i++) {
        const char * s = pr_str(v->buf[i]);
        char_count += strlen(s);
        element_count++;
        string_node = list_extend(mal_str(s), string_node);
        if (string_head == NULL) {
            string_head = string_node;
        }
    }
    return join_strings(string_head, char_count, element_count, true);
}

// return a string representation of the mal value
const char *pr_str(mal m)
{
    int buf_size;
    char *buf;

    switch (m.tag) {
        case MISSING:
            return "Internal error - pr_str on a missing value";
        case EXCEPTION:
            return pr_str(*(m.e));
        case TRUE:
            return "true";
        case FALSE:
            return "false";
        case NIL:
            return "nil";
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
        case KW:
            debug("pr_str", "kw :%s", m.s);
            buf_size = 1 + snprintf(NULL, 0, ":%s", m.s);
            buf = checked_malloc(buf_size, "pr_str KW");
            snprintf(buf, buf_size, ":%s", m.s);
            return buf;
        case LIST:
            return print_list(m.n);
        case VEC:
            return print_vec(m.v);
        default:
            internal_error("pr_str saw unknown tag");
    }
}



