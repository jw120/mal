#include <stdio.h>
#include <string.h>

#include "list.h"
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
            list_node *input_head = m.n;
            list_node *string_head = NULL;
            int char_count = 0;
            int element_count = 0;
            list_node * input_node = input_head;
            list_node * string_node = string_head;
            while (input_node != NULL) {
                const char * s = pr_str(input_node->val);
                char_count += strlen(s);
                element_count++;
                string_node = list_extend(make_str(s), string_node);
                if (string_head == NULL) {
                    string_head = string_node;
                }
                input_node = input_node->next;
            }
            buf_size = 1 + 2 + char_count + (element_count - 1);
            buf = checked_malloc(buf_size, "pr_str LIST");
            str_concat(buf, "(", buf_size - 1);
            for (string_node = string_head; string_node != NULL; string_node = string_node->next) {
                str_concat(buf, string_node->val.s, buf_size - 1);
                if (string_node->next != NULL) {
                    str_concat(buf, " ", buf_size - 1);
                }
            }
            str_concat(buf, ")", buf_size - 1);
            return buf;
    }
}

