/**
 *
 * printer.c - provides the pr_str to convert a mal type to a character string
 *
 **/

#include <stdio.h>
#include <string.h>

#include "printer.h"

#include "debug.h"
#include "hash_table.h"
#include "seq.h"
#include "utils.h"

typedef enum { PRINT_LIST, PRINT_VEC, PRINT_MAP } join_mode;

// Print a list
static const char *print_list(list_node *input_head, bool print_readably) {
  list_node *string_head = NULL;
  size_t char_count = 0;
  count_t element_count = 0;
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
  DEBUG_INTERNAL_FMT("returns '%s'", str_join(string_head, char_count,
                                              element_count, " ", "(", ")"));
  return str_join(string_head, char_count, element_count, " ", "(", ")");
}

// Print a map
static const char *print_map(hash_table *ht, bool print_readably) {
  size_t char_count = 0;
  count_t element_count = 0;
  list_node *keys = ht_keys(ht);
  list_node *values = ht_values(ht);
  list_node *string_head = NULL;
  list_node *string_node = string_head;
  while (keys != NULL && values != NULL) {
    const char *s1 = pr_str(keys->val, print_readably);
    char_count += strlen(s1);
    element_count++;
    string_node = list_extend(mal_str(s1), string_node);
    string_head = (string_head == NULL) ? string_node : string_head;
    const char *s2 = pr_str(values->val, print_readably);
    char_count += strlen(s2);
    element_count++;
    string_node = list_extend(mal_str(s2), string_node);
    keys = keys->next;
    values = values->next;
  }
  return str_join(string_head, char_count, element_count, " ", "{", "}");
}

// Print a vector
static const char *print_vec(vec *v, bool print_readably) {
  if (v == NULL)
    return "[]";
  list_node *string_head = NULL;
  size_t char_count = 0;
  count_t element_count = 0;
  list_node *string_node = string_head;
  for (count_t i = 0; i < v->count; i++) {
    const char *s = pr_str(v->buf[i], print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);
    if (string_head == NULL)
      string_head = string_node;
  }
  return str_join(string_head, char_count, element_count, " ", "[", "]");
}

#define ATOM_PREFIX "(atom "
#define ATOM_SUFFIX ")"
#define EXCEPTION_PREFIX "Exception: "

// return a string representation of the mal value
const char *pr_str(mal m, bool print_readably) {
  DEBUG_INTERNAL_FMT("called with tag %d", m.tag);
  size_t buf_size;
  char *buf;
  const char *buf2;

  switch (m.tag) {
  case MISSING:
    return "Internal error - pr_str on a missing value";
  case EXCEPTION:
    buf2 = pr_str(*(m.e), false);
    buf_size = 1 + strlen(EXCEPTION_PREFIX) + strlen(buf2);
    buf = checked_malloc(buf_size, "pr_str exception");
    snprintf(buf, buf_size, "%s%s", EXCEPTION_PREFIX, buf2);
    return buf;
  case MAL_TRUE:
    return "true";
  case MAL_FALSE:
    return "false";
  case NIL:
    return "nil";
  case INT:
    DEBUG_INTERNAL_FMT("int %d", m.i);
    buf_size = 1 + (size_t)snprintf(NULL, 0, "%d", m.i);
    buf = checked_malloc(buf_size, "pr_str INT");
    snprintf(buf, buf_size, "%d", m.i);
    return buf;
  case STR_OR_KW:
    if (is_kw(m)) {
      const char *kw_s = skip_kw_prefix(m.s);
      DEBUG_INTERNAL_FMT("kw :%s", kw_s);
      buf_size = 1 + (size_t)snprintf(NULL, 0, ":%s", kw_s);
      buf = checked_malloc(buf_size - 1, "pr_str KW");
      snprintf(buf, buf_size, ":%s", kw_s);
      return buf;
    } else {
      if (!print_readably)
        return m.s;
      DEBUG_INTERNAL_FMT("str \"%s\"", m.s);
      mal esc_m = add_escapes(mal_str(m.s));
      buf_size = 1 + (size_t)snprintf(NULL, 0, "\"%s\"", esc_m.s);
      buf = checked_malloc(buf_size, "pr_str STR");
      snprintf(buf, buf_size, "\"%s\"", esc_m.s);
      return buf;
    }
  case SYM:
    DEBUG_INTERNAL_FMT("sym %s", m.s);
    return m.s;
  case LIST:
    DEBUG_INTERNAL_FMT("starting print_list");
    return print_list(m.n, print_readably);
  case VEC:
    return print_vec(m.v, print_readably);
  case MAP:
    return print_map(m.m, print_readably);
  case FN:
  case CLOSURE:
    return "<function>";
  case ATOM:
    buf2 = pr_str(**m.a, print_readably);
    buf_size = strlen(ATOM_PREFIX) + strlen(buf2) + strlen(ATOM_SUFFIX) + 1;
    buf = checked_malloc(buf_size, "pr_str atom");
    snprintf(buf, buf_size, "%s%s%s", ATOM_PREFIX, buf2, ATOM_SUFFIX);
    return buf;
  }
}
