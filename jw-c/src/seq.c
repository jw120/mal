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
    if (m.tag == LIST) {
        return list_count(m.n);
    } else if (m.tag == VEC) {
        return vec_count(m.v);
    }
    return 0;
}

// Is the sequence empty
bool seq_empty(mal m) {
    if (m.tag == LIST) {
        return list_empty(m.n);
    } else if (m.tag == VEC) {
        return vec_empty(m.v);
    }

    return true;
}

// Helper function to compare a list to a vector
static bool list_vec_equals(list_node *n, vec *v) {
    printf("list_vec_equals comparing n=%p and v=%p\n", n, v);
    printf("list null? %s vec len %d\n", n == NULL ? "T" : "F", v->size);
    if (n == NULL && v == NULL) {
        return true;
    }
    if (n == NULL) {
        return v == NULL || v->size == 0;
    }
    if (v == NULL) {
        return n == NULL;
    }
    int i = 0;
    while (i < v->size && n != NULL) {
        if (!mal_equals(n->val, v->buf[i])) {
            return false;
        }
        i++;
        n = n->next;
    }
    return i == v->size && n == NULL;
}

// Are the two sequences equal
bool seq_equals(mal x, mal y) {
    printf("seq_equals comparing %d %d\n", x.tag, y.tag);
    if (x.tag == LIST && y.tag == LIST) {
        return list_equals(x.n, y.n);
    }
    if (x.tag == VEC && y.tag == VEC) {
        return vec_equals(x.v, y.v);
    }
    if (x.tag == LIST && y.tag == VEC) {
        return list_vec_equals(x.n, y.v);
    }
    if (x.tag == VEC && y.tag == LIST) {
        return list_vec_equals(y.n, x.v);
    }
    return false;
}



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

bool vec_equals(vec *a, vec *b) {
    if (a == NULL && b == NULL) {
        return true;
    }
    int a_size = a == NULL ? 0 : a->size;
    int b_size = b == NULL ? 0 : b->size;
    if (a->size != b_size) {
        return false;
    }
    for (int i=0; i < a_size; i++) {
        if (!mal_equals(a->buf[i], b->buf[i])) {
            return false;
        }
    }
    return true;
}

// Create a vector of the given size with elements from the given list
vec *create_vec(size_t count, list_node *n) {
    vec *v = checked_malloc(sizeof(vec), "create_vec vec");
    v->size = count;
    v->buf = checked_malloc(count* sizeof(mal), "create_vec");
    mal *buf_ptr = v->buf;
    while (n != NULL) {
        *buf_ptr = n->val;
        buf_ptr++;
        n = n-> next;

    }
    return v;
}