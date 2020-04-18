/**
 *
 * list.c - provides a lisp-like singly linked lists and a counted vector type
 *
 **/

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "debug.h"
#include "seq.h"

#include "utils.h"

/**
 *
 * Sequence functions (for both a list of vector)
 *
 */

// Count the elements in a sequence
count_t seq_count(mal m) {
  if (is_list(m))
    return list_count(m.n);
  else if (is_vec(m))
    return vec_count(m.v);
  return 0;
}

// Is the sequence empty
bool seq_empty(mal m) {
  if (is_list(m))
    return list_empty(m.n);
  else if (is_vec(m))
    return vec_empty(m.v);
  return true;
}

bool list_vec_equals(list_node *n, vec *v) {
  if (n == NULL && v == NULL)
    return true;
  if (n == NULL)
    return v == NULL || v->count == 0;
  if (v == NULL)
    return n == NULL;

  // check element by element
  count_t i = 0;
  while (i < v->count && n != NULL) {
    if (!mal_equals(n->val, v->buf[i]))
      return false;
    i++;
    n = n->next;
  }
  return i == v->count && n == NULL;
}

list_node *seq_to_list(mal m) {
  assert(is_list(m) || is_vec(m));
  if (is_list(m))
    return m.n;
  list_node *n = NULL;
  for (count_t i = 0; i < m.v->count; i++) {
    n = list_cons(m.v->buf[m.v->count - i - 1], n);
  }
  return n;
}

/**
 *
 * List functions
 *
 */

// Length of the list
count_t list_count(list_node *n) {
  count_t count = 0;
  while (n != NULL) {
    n = n->next;
    count++;
  }
  return count;
}

// Is the list empty
bool list_empty(list_node *n) { return n == NULL; }

bool list_equals(list_node *a, list_node *b) {
  while (true) {
    if (a == NULL && b == NULL)
      return true;
    if (a == NULL || b == NULL)
      return false;
    if (!mal_equals(a->val, b->val))
      return false;
    a = a->next;
    b = b->next;
  }
}

// return a new list whose head is the given element and whose tail is the
// given list (which may be NULL)
list_node *list_cons(mal m, list_node *n) {
  list_node *new_list_node = checked_malloc(sizeof(list_node), "list_cons");
  new_list_node->val = m;
  new_list_node->next = n;
  return new_list_node;
}

// generate a list from an array of mal values
list_node *array_to_list(count_t count, mal a[]) {
  list_node *n = NULL;
  for (count_t i = 0; i < count; i++)
    n = list_cons(a[count - i - 1], n);
  return n;
}

mal mal_cons(mal m, mal n) {
  if (!is_list(n))
    return mal_exception_str("non-list in mal_cons");
  return mal_list(list_cons(m, n.n));
}

mal mal_first(mal m) {
  DEBUG_HIGH_MAL("called with", m);
  if (is_list(m)) {
    if (m.n == NULL)
      return mal_nil();
    return m.n->val;
  }
  if (is_vec(m)) {
    if (m.v == NULL || m.v->count == 0)
      return mal_nil();
    return m.v->buf[0];
  }
  if (is_nil(m))
    return m;
  return mal_exception_str("non-sequence in mal_first");
}

mal mal_rest(mal m) {
  DEBUG_HIGH_MAL("called with", m);
  if (is_list(m)) {
    if (m.n == NULL)
      return mal_list(NULL);
    return mal_list(m.n->next);
  }
  if (is_vec(m)) {
    if (m.v == NULL || m.v->count < 2)
      return mal_list(NULL);
    return mal_rest(mal_list(array_to_list(m.v->count, m.v->buf)));
  }
  if (is_nil(m))
    return mal_list(NULL);
  return mal_exception_str("non-sequence in mal_first");
}

bool is_pair(mal m) { return is_seq(m) && seq_count(m) > 0; }

// Does the list include the given element
bool list_contains(list_node *n, mal m) {
  while (n != NULL) {
    if (mal_equals(n->val, m))
      return true;
    n = n->next;
  }
  return false;
}

// Given a pointer to the last element of a list (or NULL), add the given
// element and return the new last element
list_node *list_extend(mal m, list_node *n) {
  list_node *new_list_node = checked_malloc(sizeof(list_node), "list_cons");
  new_list_node->val = m;
  new_list_node->next = NULL;
  if (n != NULL)
    n->next = new_list_node;
  return new_list_node;
}

// Create a new list consisting of (at most) the given number of elements
list_node *list_take(list_node *n, count_t count) {
  count_t i = 0;
  list_node *prev_n = NULL;
  list_node *head = NULL;
  list_node *new_n = NULL;
  while (n != NULL && i < count) {
    new_n = checked_malloc(sizeof(list_node), "list_take");
    new_n->val = n->val;
    new_n->next = NULL;
    if (prev_n != NULL)
      prev_n->next = new_n;
    if (head == NULL)
      head = new_n;
    prev_n = new_n;
    n = n->next;
    i++;
  }
  return head;
}

// Return the last value in  a list
mal list_last(list_node *n) {
  if (n == NULL)
    return mal_exception_str("Last element of empty list");
  while (n->next != NULL)
    n = n->next;
  return n->val;
}

// Create a new list that combines both lists
list_node *list_append(list_node *n1, list_node *n2) {
  list_node *prev_n = NULL;
  list_node *head = NULL;
  list_node *new_n = NULL;
  while (n1 != NULL) {
    new_n = checked_malloc(sizeof(list_node), "list_append 1");
    new_n->val = n1->val;
    new_n->next = NULL;
    if (prev_n != NULL)
      prev_n->next = new_n;
    if (head == NULL)
      head = new_n;
    prev_n = new_n;
    n1 = n1->next;
  }
  while (n2 != NULL) {
    new_n = checked_malloc(sizeof(list_node), "list_append 2");
    new_n->val = n2->val;
    new_n->next = NULL;
    if (prev_n != NULL)
      prev_n->next = new_n;
    if (head == NULL)
      head = new_n;
    prev_n = new_n;
    n2 = n2->next;
  }
  return head;
}

/**
 *
 * Vector functions
 *
 */

count_t vec_count(vec *v) { return v == NULL ? 0 : v->count; }

bool vec_empty(vec *v) { return v == NULL ? true : (v->count == 0); }

bool vec_equals(vec *a, vec *b) {
  if (a == NULL && b == NULL)
    return true;
  count_t a_size = a == NULL ? 0 : a->count;
  count_t b_size = b == NULL ? 0 : b->count;
  if (a->count != b_size)
    return false;
  for (count_t i = 0; i < a_size; i++)
    if (!mal_equals(a->buf[i], b->buf[i]))
      return false;
  return true;
}

// Create a vector with unititialized entries
vec *uninitialized_vec(count_t count) {
  vec *v = checked_malloc(sizeof(vec), "uninitialized_vec vec");
  v->count = count;
  v->buf = checked_malloc(count * sizeof(mal), "uninitialized_vec buf");
  return v;
}

// Create a vector of the given size with elements from the given list
vec *list_to_vec(count_t count, list_node *n) {
  vec *v = uninitialized_vec(count);
  mal *buf_ptr = v->buf;
  while (n != NULL) {
    *buf_ptr = n->val;
    buf_ptr++;
    n = n->next;
  }
  return v;
}
