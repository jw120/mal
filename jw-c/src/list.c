#include <stdbool.h>
#include <stddef.h>

#include "list.h"
#include "utils.h"

// return a new list whose head is the given element and whose tail is the given list (which may be NULL)
list_node *list_cons(mal m , list_node *n) {
    list_node *new_list_node = checked_malloc(sizeof (list_node), "list_cons");
    new_list_node->val = m;
    new_list_node->next = n;
    return new_list_node;
}

// Given a pointer to the last element of a list (or NULL), add the given element and return the new
// last element
list_node *list_extend(mal m, list_node *n) {
    list_node *new_list_node = checked_malloc(sizeof (list_node), "list_cons");
    new_list_node->val = m;
    new_list_node->next = NULL;
    if (n != NULL) {
        n -> next = new_list_node;
    }
    return new_list_node;
}

// Length of the list
int list_len(list_node *n) {
    int count = 0;
    while (n != NULL) {
        n = n -> next;
        count++;
    }
    return count;
}

// Is the list empty
bool list_empty(list_node *n) {
    return n == NULL;
}

// Are the two lists equal
bool list_equals(list_node *a, list_node *b) {
    while (true) {
        if (a == NULL && b == NULL) {
            return true;
        }
        if (a == NULL || b == NULL) {
        return false;
        }
        if (!mal_equals(a->val, b->val)) {
            return false;
        }
        a = a->next;
        b = b->next;
    }
}

