/**
 *
 * printer.c - provides the pr_str to convert a mal type to a character string
 *
 **/

#include <stdio.h>
#include <string.h>

#include "printer.h"

#include "map.h"
#include "seq.h"
#include "utils.h"

#define BUFFER_SIZE_FOR_INT 16

typedef enum
{
  PRINT_LIST,
  PRINT_VEC,
  PRINT_MAP
} join_mode;

// Helper function to concat a list of strings with spaces and surround with
// (..)  [..] {..}
static const char *join_strings(list_node *s, int chars, int elements,
                                join_mode mode)
{

  const char *opener;
  const char *closer;
  switch (mode)
  {
  case PRINT_LIST:
    opener = "(";
    closer = ")";
    break;
  case PRINT_VEC:
    opener = "[";
    closer = "]";
    break;
  case PRINT_MAP:
    opener = "{";
    closer = "}";
    break;
  }

  int num_spaces = elements > 0 ? elements - 1 : 0;
  int buf_size = 1 + 2 + chars + num_spaces;
  char *buf = checked_malloc(buf_size, "join_string");

  str_concat(buf, opener, buf_size - 1);
  while (s != NULL)
  {
    str_concat(buf, s->val.s, buf_size - 1);
    if (s->next != NULL)
    {
      str_concat(buf, " ", buf_size - 1);
    }
    s = s->next;
  }
  str_concat(buf, closer, buf_size - 1);
  return buf;
}

// Print a list
static const char *print_list(list_node *input_head, bool print_readably)
{
  debug("pr_str", "print_list %p", input_head);
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *input_node = input_head;
  list_node *string_node = string_head;
  while (input_node != NULL)
  {
    const char *s = pr_str(input_node->val, print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);
    debug("pr_str", "list_extend %s", s);

    if (string_head == NULL)
    {
      string_head = string_node;
    }
    input_node = input_node->next;
  }
  return join_strings(string_head, char_count, element_count, PRINT_LIST);
}

// Print a map
static const char *print_map(map *m, bool print_readably)
{
  debug("pr_str", "print_map %p", m);
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *string_node = string_head;
  for (int i = 0; i < m->size; i++)
  {
    mal key =
        m->table[i].is_kw ? mal_kw(m->table[i].key) : mal_str(m->table[i].key);
    const char *s1 = pr_str(key, print_readably);
    char_count += strlen(s1);
    element_count++;
    string_node = list_extend(mal_str(s1), string_node);
    debug("pr_str", "list_extend %s", s1);
    string_head = (string_head == NULL) ? string_node : string_head;
    const char *s2 = pr_str(m->table[i].val, print_readably);
    char_count += strlen(s2);
    element_count++;
    string_node = list_extend(mal_str(s2), string_node);
    debug("pr_str", "list_extend %s", s2);
  }
  return join_strings(string_head, char_count, element_count, PRINT_MAP);
}

// Print a vector
static const char *print_vec(vec *v, bool print_readably)
{
  debug("pr_str", "print_vec %p", v);
  if (v == NULL)
  {
    return "[]";
  }
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
  list_node *string_node = string_head;
  for (int i = 0; i < v->size; i++)
  {
    const char *s = pr_str(v->buf[i], print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);
    if (string_head == NULL)
    {
      string_head = string_node;
    }
  }
  return join_strings(string_head, char_count, element_count, PRINT_VEC);
}

// return a string representation of the mal value
const char *pr_str(mal m, bool print_readably)
{
  int buf_size;
  char *buf;

  switch (m.tag)
  {
  case MISSING:
    return "Internal error - pr_str on a missing value";
  case EXCEPTION:
    return pr_str(*(m.e), print_readably);
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
    mal esc_m = print_readably ? add_escapes(mal_str(m.s)) : m;
    buf_size = 1 + snprintf(NULL, 0, "\"%s\"", esc_m.s);
    buf = checked_malloc(buf_size, "pr_str STR");
    snprintf(buf, buf_size, "\"%s\"", esc_m.s);
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
    return print_list(m.n, print_readably);
  case VEC:
    return print_vec(m.v, print_readably);
  case MAP:
    return print_map(m.m, print_readably);
  case FN:
    return "<function>";
  }
}
