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
mal equals(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  RETURN_IF_EXCEPTION(n->val);
  RETURN_IF_EXCEPTION(n->next->val);
  return mal_bool(mal_equals(n->val, n->next->val));
}

// Helper function
static const char *print_and_join_list(list_node *n, env *e,
                                       bool print_readably, const char *sep) {
  list_node *string_head = NULL;
  int char_count = 0;
  int element_count = 0;
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
mal mal_pr_str(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, e, true, " "));
}

// C implementation of mal prn: calls pr_str on each argument with
// print_readably set to true, joins the results with " ", prints the string to
// the screen and then returns nil.
mal prn(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, e, true, " "));
  return mal_nil();
}

// C implementation of mal str: calls pr_str on each argument with
// print_readably set to false, concatenates the results together (""
// separator), and returns the new string.
mal mal_fn_str(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, e, false, ""));
}

// C implementation of mal println: calls pr_str on each argument with
// print_readably set to false, joins the results with " ", prints the string to
// the screen and then returns nil.
mal println(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, e, false, " "));
  return mal_nil();
}

// C implementation of mal read-string
mal read_string(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("Bad arguments to read-string");
  return read_str(n->val.s);
}

#define SLURP_BUFFER_SIZE 65536

// C implementation of mal slurp
mal slurp(list_node *n, env *e) {
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

// add misc core functions to the environment
void add_misc(env *e) {
  env_set(e, "prn", mal_fn(prn));
  env_set(e, "pr-str", mal_fn(mal_pr_str));
  env_set(e, "str", mal_fn(mal_fn_str));
  env_set(e, "println", mal_fn(println));
  env_set(e, "=", mal_fn(equals));
  env_set(e, "read-string", mal_fn(read_string));
  env_set(e, "slurp", mal_fn(slurp));
}
