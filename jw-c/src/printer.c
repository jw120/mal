/**
 *
 * printer.c - provides the pr_str to convert a mal type to a character string
 *
 **/

#include <stdio.h>
#include <string.h>

#include "printer.h"

#include "debug.h"
#include "map.h"
#include "seq.h"
#include "utils.h"

#define BUFFER_SIZE_FOR_INT 16

typedef enum { PRINT_LIST, PRINT_VEC, PRINT_MAP } join_mode;

// Print a list
static const char *print_list(list_node *input_head, bool print_readably) {
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *input_node = input_head;
  list_node *string_node = string_head;
  while (input_node != NULL) {
    const char *s = pr_str(input_node->val, print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);

    if (string_head == NULL)
      string_head = string_node;
    input_node = input_node->next;
  }
  return str_join(string_head, char_count, element_count, " ", "(", ")");
}

// Print a map
static const char *print_map(map *m, bool print_readably) {
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *string_node = string_head;
  for (int i = 0; i < m->size; i++) {
    mal key =
        m->table[i].is_kw ? mal_kw(m->table[i].key) : mal_str(m->table[i].key);
    const char *s1 = pr_str(key, print_readably);
    char_count += strlen(s1);
    element_count++;
    string_node = list_extend(mal_str(s1), string_node);
    string_head = (string_head == NULL) ? string_node : string_head;
    const char *s2 = pr_str(m->table[i].val, print_readably);
    char_count += strlen(s2);
    element_count++;
    string_node = list_extend(mal_str(s2), string_node);
  }
  return str_join(string_head, char_count, element_count, " ", "{", "}");
}

// Print a vector
static const char *print_vec(vec *v, bool print_readably) {
  if (v == NULL)
    return "[]";
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *string_node = string_head;
  for (int i = 0; i < v->size; i++) {
    const char *s = pr_str(v->buf[i], print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);
    if (string_head == NULL)
      string_head = string_node;
  }
  return str_join(string_head, char_count, element_count, " ", "[", "]");
}

// return a string representation of the mal value
const char *pr_str(mal m, bool print_readably) {
  int buf_size;
  char *buf;

  switch (m.tag) {
  case MISSING:
    return "Internal error - pr_str on a missing value";
  case EXCEPTION:
    return pr_str(*(m.e), false);
  case TRUE:
    return "true";
  case FALSE:
    return "false";
  case NIL:
    return "nil";
  case INT:
    DEBUG_INTERNAL_FMT("int %d", m.i);
    buf_size = 1 + snprintf(NULL, 0, "%d", m.i);
    buf = checked_malloc(buf_size, "pr_str INT");
    snprintf(buf, buf_size, "%d", m.i);
    return buf;
  case STR:
    if (!print_readably)
      return m.s;
    DEBUG_INTERNAL_FMT("str \"%s\"", m.s);
    mal esc_m = add_escapes(mal_str(m.s));
    buf_size = 1 + snprintf(NULL, 0, "\"%s\"", esc_m.s);
    buf = checked_malloc(buf_size, "pr_str STR");
    snprintf(buf, buf_size, "\"%s\"", esc_m.s);
    return buf;
  case SYM:
    DEBUG_INTERNAL_FMT("sym %s", m.s);
    return m.s;
  case KW:
    DEBUG_INTERNAL_FMT("kw :%s", m.s);
    buf_size = 1 + snprintf(NULL, 0, ":%s", m.s);
    buf = checked_malloc(buf_size - 1, "pr_str KW");
    snprintf(buf, buf_size, ":%s", m.s);
    return buf;
  case LIST:
    return print_list(m.n, print_readably);
  case VEC:
    return print_vec(m.v, print_readably);
  case MAP:
    return print_map(m.m, print_readably);
  case FN:
  case CLOSURE:
    return "<function>";
  }
}
