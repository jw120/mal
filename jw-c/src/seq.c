/**
 *
 * list.c - provides a lisp-like singly linked lists and a counted vector type
 *
 **/


#include <stdbool.h>
#include <stddef.h>

#include "seq.h"
#include "utils.h"

/**
 *
 * Sequence functions (for both a list of vector)
 *
 */

// Count the elements in a sequence
int seq_count(mal m) {
    return 0; // NYI
}

// Is the sequence empty
bool seq_empty(mal m) {
    return true; // NYI
}

// Are the two sequences equal
bool seq_equals(mal x, mal y) {
    return true; // NYI
}

// bool list_equals(list_node *a, list_node *b) {
//     while (true) {
//         if (a == NULL && b == NULL) {
//             return true;
//         }
//         if (a == NULL || b == NULL) {
//         return false;
//         }
//         if (!mal_equals(a->val, b->val)) {
//             return false;
//         }
//         a = a->next;
//         b = b->next;
//     }
// }

/**
 *
 * List functions
 *
 */

// Length of the list
int list_count(list_node *n) {
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

bool list_equals(list_node *x, list_node *y) {
    return true; // NYI
}

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

/**
 *
 * Vector functions
 *
 */

int vec_count(vec *v) {
    return v == NULL ? 0 : v->size;
}

bool vec_empty(vec *v) {
    return v == NULL ? true : (v->size == 0);
}

// Create a vector of the given size with elements from the given list
vec *create_vec(size_t count, list_node *n) {
    return NULL; // NYI
}


//    mal *buf = checked_malloc(nodes * sizeof(mal), "read_seq");
//     mal *node_ptr = head;
//     mal *buf_ptr = buf;
//     while (node_ptr != NULL) {
//         *buf_ptr = *node_ptr;
//         node_ptr = node_ptr->next;
//         buf_ptr++;
//     }

