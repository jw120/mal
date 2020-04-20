/**
 *
 * eval.c - provides the evaluator
 *
 * Note that we are using an exception tag in mal to simulate exceptions, so
 * we need to actively look for exception returns from every eval and propogate
 *them
 *
 **/

#include "eval.h"

#include "debug.h"
#include "env.h"
#include "hash_table.h"
#include "seq.h"
#include "utils.h"

// Is the argument a list whose first element is a symbol that
// looks up to a macro call
static bool is_macro_call(mal ast, env *e) {
  DEBUG_INTERNAL_MAL("called with", ast);
  if (!is_list(ast))
    return false;
  mal head = mal_first(ast);
  if (!is_sym(head))
    return false;
  mal head_lookup = env_get(e, head.s);
  bool return_val = is_closure(head_lookup) && head_lookup.c->is_macro;
  DEBUG_INTERNAL_FMT("returning %s", return_val ? "TRUE" : "FALSE");
  return return_val;
}

// Macroexpand (repeatedly) the ast
static mal macroexpand(mal ast, env *e) {
  DEBUG_INTERNAL_MAL("called with", ast);
  while (is_macro_call(ast, e)) {
    mal head = mal_first(ast);
    mal rest = mal_rest(ast);
    if (!is_sym(head))
      internal_error("not a symbol in macroexpand");
    mal head_lookup = env_get(e, head.s);
    DEBUG_INTERNAL_MAL("head_lookup is", head_lookup);
    DEBUG_INTERNAL_MAL("rest is", rest);
    DEBUG_INTERNAL_MAL("binds is", mal_list(head_lookup.c->binds));
    e = env_new2(head_lookup.c->binds, rest.n, head_lookup.c->e);
    if (e == NULL)
      return mal_exception_str("Failed to create environment for macro");
    ast = eval(head_lookup.c->body, e);
    DEBUG_INTERNAL_MAL("expanded ast is", ast);
  }
  return ast;
}

// Handle the def! special form
static mal def_special_form(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL || !is_sym(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to def!");
  mal evaluated_val = eval(n->next->val, e);
  RETURN_IF_EXCEPTION(evaluated_val);
  env_set(e, n->val.s, evaluated_val);
  return evaluated_val;
}

// Handle the defmacro! special form
static mal defmacro_special_form(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL || !is_sym(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to defmacro!");
  mal evaluated_val = eval(n->next->val, e);
  RETURN_IF_EXCEPTION(evaluated_val);
  if (!is_closure(evaluated_val) || is_macro(evaluated_val))
    return mal_exception_str("defmacro! needs a function");
  // copy closure, setting is_macro
  closure *new_closure = checked_malloc(sizeof(closure), "defmacro");
  new_closure->binds = evaluated_val.c->binds;
  new_closure->body = evaluated_val.c->body;
  new_closure->e = evaluated_val.c->e;
  new_closure->is_macro = true;
  mal new_val = mal_closure(new_closure);
  env_set(e, n->val.s, new_val);
  return new_val;
}

// Handle the do special form. Last form is returned without
// evaluation to be handled in eval()'s TCO loop
static mal do_special_form_before_continue(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL)
    return mal_exception_str("Bad arguments to do");
  while (n != NULL && n->next != NULL) {
    mal ret = eval(n->val, e);
    RETURN_IF_EXCEPTION(ret);
    n = n->next;
  }
  return n->val;
}

// Handle the fn* special form
static mal fn_special_form(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (list_count(n) != 2 || !is_seq(n->val))
    return mal_exception_str("Bad arguments to fn*");
  closure *c = checked_malloc(sizeof(closure), "fn*");
  c->binds = seq_to_list(n->val);
  c->body = n->next->val;
  c->e = e;
  c->is_macro = false;
  DEBUG_HIGH_MAL("in fn* closure body is", c->body);
  return mal_closure(c);
}

// Handle the if special form, returning an ast for further evaluation
// in eval()'s TCO loop
static mal if_special_form_before_continue(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL || list_count(n) < 2 || list_count(n) > 3)
    return mal_exception_str("Bad arguments to if");
  mal condition = eval(n->val, e);
  mal if_ast = n->next->val;
  mal else_ast = (n->next->next == NULL) ? mal_nil() : n->next->next->val;
  if (is_exception(condition)) {
    return condition;
  } else if (is_falsey(condition)) {
    return else_ast;
  } else {
    return if_ast;
  }
}

// Handle the let* special form, returning an ast for further evaluation
// in eval()'s TCO loop and modifying the environment
static mal let_special_form_before_continue(list_node *n, env **eptr) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Bad arguments to let*");
  RETURN_IF_EXCEPTION(n->val);
  RETURN_IF_EXCEPTION(n->next->val);
  if (!is_seq(n->val) || seq_count(n->val) % 2)
    return mal_exception_str("Bad arguments to let*");

  // Create our new enviornment for the let*
  *eptr = env_new(NULL, *eptr);
  if (is_list(n->val)) {
    for (list_node *p = n->val.n; p != NULL; p = p->next->next) {
      mal sym = p->val;
      RETURN_IF_EXCEPTION(sym);
      if (!is_sym(sym))
        return mal_exception_str("Non-symbol in binding list in let");
      mal val = p->next->val;
      mal evaluated_val = eval(val, *eptr);
      RETURN_IF_EXCEPTION(evaluated_val);
      env_set(*eptr, sym.s, evaluated_val);
    }
  } else { // vector
    for (count_t i = 0; i < n->val.v->count; i += 2) {
      mal sym = n->val.v->buf[i];
      RETURN_IF_EXCEPTION(sym);
      if (!is_sym(sym))
        return mal_exception_str("Non-symbol in binding vector in let");
      mal val = n->val.v->buf[i + 1];
      mal evaluated_val = eval(val, *eptr);
      RETURN_IF_EXCEPTION(evaluated_val);
      env_set(*eptr, sym.s, evaluated_val);
    }
  }
  return n->next->val;
}

// Handle the try* special form
static mal try_special_form(list_node *n, env *e) {
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (list_count(n) == 1) // try* without catch*
    return eval(n->val, e);
  if (list_count(n) != 2)
    return mal_exception_str("Bad arguments to try*");

  mal try_value = eval(n->val, e);
  if (!is_exception(try_value))
    return try_value;

  mal catch_list = n->next->val;
  if (!is_list(catch_list) || list_count(catch_list.n) != 3 ||
      !mal_equals(catch_list.n->val, mal_sym("catch*")) ||
      !is_sym(catch_list.n->next->val))
    return mal_exception_str("Bad catch* clause for try");
  mal catch_env_list = mal_cons(catch_list.n->next->val,
                                mal_cons(*(try_value.e), mal_list(NULL)));
  env *catch_env = env_new(catch_env_list.n, e);
  return eval(catch_list.n->next->next->val, catch_env);
}

// Helper functin for quasi-quote is the argument a non-empty sequence
bool is_pair(mal m) {
  return (is_list(m) && !list_empty(m.n)) || (is_vec(m) && !vec_empty(m.v));
}

// Handle the quasiquote special form (calls itself recursively)
static mal quasiquote(mal ast) {
  DEBUG_INTERNAL_MAL("", ast);

  // a non-sequence (or empty sequence) is just quoted
  if (!is_pair(ast))
    return mal_cons(mal_sym("quote"), mal_cons(ast, mal_list(NULL)));

  // if a list then we work with the head (ast_head)
  mal ast_head = mal_first(ast);
  DEBUG_INTERNAL_MAL("ast_head is", ast_head);

  // unquote
  if (mal_equals(ast_head, mal_sym("unquote"))) {
    if (seq_count(ast) != 2)
      return mal_exception_str("Bad arguments to unquote");
    return is_list(ast) ? ast.n->next->val : ast.v->buf[1];
  }

  mal ast_rest = mal_rest(ast);
  DEBUG_INTERNAL_MAL("ast_rest is", ast_rest);

  // splice-unquote the element with concat
  if (is_pair(ast_head)) {
    mal ast_head_head = ast_head.n->val;
    if (mal_equals(ast_head_head, mal_sym("splice-unquote"))) {
      if (ast_head.n->next == NULL)
        return mal_exception_str("Missing argument for splice-unquote");
      return mal_cons(mal_sym("concat"),
                      mal_cons(ast_head.n->next->val,
                               mal_cons(quasiquote(ast_rest), mal_list(NULL))));
    }
  }

  // or else just cons the element
  return mal_cons(mal_sym("cons"),
                  mal_cons(quasiquote(ast_head),
                           mal_cons(quasiquote(ast_rest), mal_list(NULL))));
}

mal eval_ast(mal ast, env *e) {
  DEBUG_INTERNAL_MAL("called with", ast);

  if (is_sym(ast)) { // lookup a symbol
    return env_get(e, ast.s);
  }

  if (is_list(ast) && !list_empty(ast.n)) { // evaluate list's contents
    list_node *to_head = NULL;
    list_node *to = to_head;
    list_node *from = ast.n;
    while (from != NULL) {
      mal m = eval(from->val, e);
      RETURN_IF_EXCEPTION(m);
      to = list_extend(m, to);
      if (to_head == NULL)
        to_head = to;
      from = from->next;
    }
    return mal_list(to_head);
  }

  if (is_vec(ast) && !list_empty(ast.n)) { // evaluate vector's contents
    vec *to = uninitialized_vec(ast.v->count);
    for (count_t i = 0; i < ast.v->count; i++) {
      mal m = eval(ast.v->buf[i], e);
      RETURN_IF_EXCEPTION(m);
      to->buf[i] = m;
    }
    return mal_vec(to);
  }

  if (is_map(ast)) { // evaluate a map's values
    hash_table *old_ht = ast.m;
    hash_table *new_ht = ht_new(old_ht->entries);
    for (list_node *n = ht_keys(old_ht); n != NULL; n = n->next) {
      const char *old_key = n->val.s;
      mal old_val = ht_get(old_ht, old_key);
      mal new_val = eval(old_val, e);
      RETURN_IF_EXCEPTION(new_val);
      ht_put(new_ht, old_key, new_val);
    }
    return mal_map(new_ht);
  }

  return ast; // Otherwise just return the atom
}

mal eval(mal ast, env *e) {

  while (true) { // loop to enable TCO
    DEBUG_HIGH_MAL("called with", ast);
    RETURN_IF_EXCEPTION(ast);
    DEBUG_HIGH_ENV(e);

    ast = macroexpand(ast, e);
    RETURN_IF_EXCEPTION(ast);
    DEBUG_INTERNAL_MAL("macro expanded", ast);

    if (!is_list(ast) || list_empty(ast.n))
      return eval_ast(ast, e);

    // Check for special forms
    mal head = mal_first(ast);
    mal rest = mal_rest(ast);
    RETURN_IF_EXCEPTION(head);
    if (mal_equals(head, mal_sym("def!")))
      return def_special_form(rest.n, e);
    if (mal_equals(head, mal_sym("defmacro!")))
      return defmacro_special_form(rest.n, e);
    if (mal_equals(head, mal_sym("do"))) {
      ast = do_special_form_before_continue(rest.n, e);
      continue;
    }
    if (mal_equals(head, mal_sym("fn*")))
      return fn_special_form(rest.n, e);
    if (mal_equals(head, mal_sym("if"))) {
      ast = if_special_form_before_continue(rest.n, e);
      continue;
    }
    if (mal_equals(head, mal_sym("let*"))) {
      ast = let_special_form_before_continue(rest.n, &e);
      continue;
    }
    if (mal_equals(head, mal_sym("macroexpand"))) {
      if (list_count(rest.n) != 1)
        return mal_exception_str("Bad arguments to macroexpand");
      return macroexpand(rest.n->val, e);
    }
    if (mal_equals(head, mal_sym("quasiquote"))) {
      if (list_count(rest.n) != 1)
        return mal_exception_str("Bad arguments to quasiquote");
      ast = quasiquote(rest.n->val);
      continue;
    }
    if (mal_equals(head, mal_sym("quote"))) {
      if (list_count(rest.n) != 1)
        return mal_exception_str("Bad arguments to quote");
      return rest.n->val;
    }
    if (mal_equals(head, mal_sym("try*"))) {
      return try_special_form(rest.n, e);
    }

    // Evaluate all the list elements
    mal evaluated_ast = eval_ast(ast, e);
    RETURN_IF_EXCEPTION(evaluated_ast);
    DEBUG_INTERNAL_MAL("after args evaluated have", evaluated_ast);

    // Apply the function
    head = mal_first(evaluated_ast);
    rest = mal_rest(evaluated_ast);
    DEBUG_INTERNAL_MAL("head of list to be applied", head);
    DEBUG_INTERNAL_MAL("rest of list to be applied", rest);
    if (is_fn(head)) // C-defined function
      return head.f(rest.n, e);
    if (is_closure(head)) { // mal-defined function
      env *closure_env = env_new2(head.c->binds, rest.n, head.c->e);
      if (closure_env == NULL)
        return mal_exception_str("Failed to create closure environment");
      e = closure_env;
      ast = head.c->body;
      continue; // TCO
    }
    return mal_exception_str("Not a function");
  }
}

mal apply(mal m, list_node *args_list, env *e) {

  if (is_fn(m)) // C-defined function
    return m.f(args_list, e);
  if (is_closure(m)) { // mal-defined function
    env *closure_env = env_new2(m.c->binds, args_list, m.c->e);
    if (closure_env == NULL)
      return mal_exception_str("Failed to create closure environment in apply");
    return eval(m.c->body, closure_env);
  }
  return mal_exception_str("Need a function or closure for apply");
}
