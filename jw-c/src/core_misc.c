/**
 *
 * core_miscc - defines misc functions for the core environment
 *
 **/

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "core_misc.h"

#include "debug.h"
#include "env.h"
#include "printer.h"
#include "reader.h"
#include "seq.h"
#include "utils.h"

// C implementation of mal =
static mal core_equals(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  RETURN_IF_EXCEPTION(n->val);
  RETURN_IF_EXCEPTION(n->next->val);
  return mal_bool(mal_equals(n->val, n->next->val));
}

// Helper function
static const char *print_and_join_list(list_node *n, UNUSED(env *e),
                                       bool print_readably, const char *sep) {
  list_node *string_head = NULL;
  size_t char_count = 0;
  count_t element_count = 0;
  list_node *string_node = string_head;
  while (n != NULL) {
    const char *s = pr_str(n->val, print_readably);
    char_count += strlen(s);
    element_count++;
    string_node = list_extend(mal_str(s), string_node);

    if (string_head == NULL)
      string_head = string_node;
    n = n->next;
  }
  return str_join(string_head, char_count, element_count, sep, "", "");
}

// C implementation of mal pr-str: calls pr_str on each argument with
// print_readably set to true, joins the results with " " and returns the new
// string.
static mal core_pr_str(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, e, true, " "));
}

// C implementation of mal prn: calls pr_str on each argument with
// print_readably set to true, joins the results with " ", prints the string to
// the screen and then returns nil.
static mal core_prn(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, e, true, " "));
  return mal_nil();
}

// C implementation of mal str: calls pr_str on each argument with
// print_readably set to false, concatenates the results together (""
// separator), and returns the new string.
static mal core_str(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, e, false, ""));
}

// C implementation of mal println: calls pr_str on each argument with
// print_readably set to false, joins the results with " ", prints the string to
// the screen and then returns nil.
static mal core_println(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, e, false, " "));
  return mal_nil();
}

// C implementation of mal read-string
static mal core_read_string(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("Bad arguments to read-string");
  return read_str(n->val.s);
}

#define SLURP_BUFFER_SIZE 65536

// C implementation of mal slurp
static mal core_slurp(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("Bad arguments to slurp");
  FILE *fp = fopen(n->val.s, "r");
  if (fp == NULL)
    return mal_exception_str(strerror(errno));
  char *buf = checked_malloc(SLURP_BUFFER_SIZE, "slurp");
  fread(buf, 1, SLURP_BUFFER_SIZE, fp);
  if (feof(fp))
    return mal_str(buf);
  return mal_exception_str("fread error in slurp");
}

// C implenetation of mal throw
static mal core_throw(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to throw");
  return mal_exception(n->val);
}

// C implenetation of mal symbol
static mal core_symbol(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("Bad arguments to symbol");
  return mal_sym(n->val.s);
}

// C implenetation of mal keyword
static mal core_keyword(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad argument number to symbol");
  if (is_str(n->val))
    return mal_kw(n->val.s);
  if (is_kw(n->val))
    return n->val;
  return mal_exception_str("Bad argument type to symbol");
}

// add misc core functions to the environment
void add_misc(env *e) {
  env_set(e, "prn", mal_fn(core_prn));
  env_set(e, "pr-str", mal_fn(core_pr_str));
  env_set(e, "str", mal_fn(core_str));
  env_set(e, "println", mal_fn(core_println));
  env_set(e, "=", mal_fn(core_equals));
  env_set(e, "read-string", mal_fn(core_read_string));
  env_set(e, "slurp", mal_fn(core_slurp));
  env_set(e, "throw", mal_fn(core_throw));
  env_set(e, "symbol", mal_fn(core_symbol));
  env_set(e, "keyword", mal_fn(core_keyword));
}
