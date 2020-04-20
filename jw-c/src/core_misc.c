/**
 *
 * core_miscc - defines misc functions for the core environment
 *
 **/

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include "core_misc.h"

#include "debug.h"
#include "env.h"
#include "eval.h"
#include "printer.h"
#include "reader.h"
#include "seq.h"
#include "utils.h"

// C implementation of mal =
static mal core_equals(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_bool(mal_equals(n->val, n->next->val));
}

// Helper function to create a string from applying pr_str to a list of values
// with the gieven separator string
static const char *print_and_join_list(list_node *n, bool print_readably,
                                       const char *sep) {
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
static mal core_pr_str(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, true, " "));
}

// C implementation of mal prn: calls pr_str on each argument with
// print_readably set to true, joins the results with " ", prints the string to
// the screen and then returns nil.
static mal core_prn(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, true, " "));
  return mal_nil();
}

// C implementation of mal str: calls pr_str on each argument with
// print_readably set to false, concatenates the results together (""
// separator), and returns the new string.
static mal core_str(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_str(print_and_join_list(n, false, ""));
}

// C implementation of mal println: calls pr_str on each argument with
// print_readably set to false, joins the results with " ", prints the string to
// the screen and then returns nil.
static mal core_println(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  puts(print_and_join_list(n, false, " "));
  return mal_nil();
}

// C implementation of mal read-string
static mal core_read_string(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("Bad arguments to read-string");
  mal m = read_str(n->val.s);
  return is_missing(m) ? mal_nil() : m;
}

#define SLURP_BUFFER_SIZE 10000000

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

// C implemetation of mal apply
mal core_apply(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) < 2)
    return mal_exception_str("Need two arguments for apply");

  // set up arguments
  list_node *middle_args = list_take(n->next, list_count(n->next) - 1);
  mal last_arg = list_last(n->next);
  list_node *args;
  if (is_seq(last_arg)) {
    args = list_append(middle_args, seq_to_list(last_arg));
  } else {
    return mal_exception_str("Last argument for apply must be a sequence");
  }

  return apply(n->val, args, e);
}

// C implementation of mal map
static mal core_map(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("map needs two arguments");
  if (!is_seq(n->next->val))
    return mal_exception_str("second map argument must be a sequence");
  if (!is_fn(n->val) && !is_closure(n->val))
    return mal_exception_str("first map arg must be a function or closure");

  list_node *args = seq_to_list(n->next->val);
  list_node *prev_n = NULL;
  list_node *new_n = NULL;
  list_node *head = NULL;
  while (args != NULL) {
    new_n = checked_malloc(sizeof(list_node), "core_map");
    if (is_fn(n->val)) {
      new_n->val = n->val.f(list_cons(args->val, NULL), e);
    } else {
      env *closure_env =
          env_new2(n->val.c->binds, list_cons(args->val, NULL), n->val.c->e);
      if (closure_env == NULL)
        return mal_exception_str("Failed to create closure environment in map");
      DEBUG_HIGH_MAL("closure body is", n->val.c->body);
      new_n->val = eval(n->val.c->body, closure_env);
    }
    if (is_exception(new_n->val))
      return new_n->val;
    new_n->next = NULL;
    if (prev_n != NULL)
      prev_n->next = new_n;
    if (head == NULL)
      head = new_n;
    prev_n = new_n;
    args = args->next;
  }
  return mal_list(head);
}

#define READLINE_BUFFER_SIZE 1024
static char readline_buffer[READLINE_BUFFER_SIZE];

// C implementation of mal readline
static mal core_readline(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_str(n->val))
    return mal_exception_str("readline takes a string");
  printf("%s ", n->val.s);
  char *result = fgets(readline_buffer, READLINE_BUFFER_SIZE, stdin);
  if (result == NULL) {
    if (feof(stdin)) { // control-d
      clearerr(stdin);
      return mal_nil();
    }
    internal_error("failure in gets");
  }
  size_t result_len = strlen(result);
  char *new_str = checked_malloc(result_len + 1, "readline");
  strncpy(new_str, result, result_len + 1);
  if (new_str[result_len - 1] == '\n') // truncate trailing newline
    new_str[result_len - 1] = '\0';
  else {
    internal_error("No newline at end of return from fgets '%s'\n", result);
  }

  return mal_str(new_str);
}

// C implementation of mal meta
static mal core_meta(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("meta takes one argument");
  return get_meta(n->val);
}

// C implementation of mal with-meta
static mal core_with_meta(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("with-meta takes two argument");
  mal old_value = n->val;
  mal new_value = mal_nil();
  mal new_meta_value = n->next->val;
  if (is_fn(old_value)) {
    new_value = mal_fn(old_value.f);
  } else if (is_closure(old_value)) {
    closure *new_closure = checked_malloc(sizeof(closure), "with-meta");
    new_closure->binds = old_value.c->binds;
    new_closure->body = old_value.c->body;
    new_closure->e = old_value.c->e;
    new_closure->is_macro = old_value.c->is_macro;
    new_value = mal_closure(new_closure);
  } else if (is_list(old_value)) {
    new_value = mal_list(old_value.n);
  } else if (is_vec(old_value)) {
    new_value = mal_vec(old_value.v);
  } else if (is_map(old_value)) {
    new_value = mal_map(old_value.m);
  } else if (is_atom(old_value)) {
    new_value = mal_atom(**(old_value.a));
  } else {
    return mal_exception_str("invalid target for with-meta");
  }
  set_meta(&(new_value), new_meta_value);
  return new_value;
}

static mal core_time_ms(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (n != NULL)
    return mal_exception_str("time-ms expects no arguments");
  struct timeval t;
  gettimeofday(&t, NULL);
  time_t milliseconds = t.tv_sec * 1000 + t.tv_usec / 1000;
  return mal_int((int)milliseconds);
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
  env_set(e, "apply", mal_fn(core_apply));
  env_set(e, "map", mal_fn(core_map));
  env_set(e, "readline", mal_fn(core_readline));
  env_set(e, "time-ms", mal_fn(core_time_ms));
  env_set(e, "meta", mal_fn(core_meta));
  env_set(e, "with-meta", mal_fn(core_with_meta));
}
