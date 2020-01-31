/**
 *
 * eval.c - provides the evaluator
 *
 * Note that we are using an exception tag in mal to simulate exceptions, so
 * we need to actively look for exception returns from every eval and propogate them
 *
 **/

#include "eval.h"

#include "assert.h"
#include "debug.h"
#include "env.h"
#include "map.h"
#include "printer.h"
#include "seq.h"
#include "utils.h"

#define RETURN_IF_EXCEPTION(x) \
  if (is_exception(x))         \
  return x

mal def_special_form(list_node *n, env *e)
{
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL || !is_sym(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to def!");
  mal evaluated_val = eval(n->next->val, e);
  RETURN_IF_EXCEPTION(evaluated_val);
  env_set(e, n->val.s, evaluated_val);
  return evaluated_val;
}

mal let_special_form(list_node *n, env *e)
{
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (n == NULL)
    return mal_exception_str("No arguments to let");
  RETURN_IF_EXCEPTION(n->val);
  if (!is_seq(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to let");
  RETURN_IF_EXCEPTION(n->next->val);
  if (seq_count(n->val) % 2)
    return mal_exception_str("Odd length argument list in let");

  // Create our new enviornment for the let*
  env *let_env = env_new(NULL, e);

  // Walk over the binding list or vector adding symbols to the new environment
  if (is_list(n->val))
  {
    list_node *p = n->val.n;
    while (p != NULL) // walk two nodes at a time
    {
      mal sym = p->val;
      RETURN_IF_EXCEPTION(sym);
      if (!is_sym(sym))
        return mal_exception_str("Non-symbol in binding list in let");
      mal val = p->next->val;
      mal evaluated_val = eval(val, let_env);
      RETURN_IF_EXCEPTION(evaluated_val);
      env_set(let_env, sym.s, evaluated_val);
      p = p->next->next;
    }
  }
  else
  {
    for (int i = 0; i < n->val.v->size; i += 2)
    {
      mal sym = n->val.v->buf[i];
      RETURN_IF_EXCEPTION(sym);
      if (!is_sym(sym))
        return mal_exception_str("Non-symbol in binding vector in let");
      mal val = n->val.v->buf[i + 1];
      mal evaluated_val = eval(val, let_env);
      RETURN_IF_EXCEPTION(evaluated_val);
      env_set(let_env, sym.s, evaluated_val);
    }
  }

  DEBUG_HIGH_ENV(let_env);
  mal ret = eval(n->next->val, let_env);
  env_free(let_env);
  return ret;
}

// evaluation function that does not apply
mal eval_ast(mal ast, env *e)
{
  DEBUG_INTERNAL_MAL("called with", ast);

  if (is_sym(ast))
  {
    return env_get(e, ast.s);
  }

  if (is_list(ast) && !seq_empty(ast))
  {
    list_node *to_head = NULL;
    list_node *to = to_head;
    list_node *from = ast.n;
    while (from != NULL)
    {
      mal m = eval(from->val, e);
      RETURN_IF_EXCEPTION(m);
      to = list_extend(m, to);
      if (to_head == NULL)
        to_head = to;
      from = from->next;
    }
    return mal_list(to_head);
  }

  if (is_vec(ast) && !seq_empty(ast))
  {
    vec *to = uninitialized_vec(ast.v->size);
    for (int i = 0; i < ast.v->size; i++)
    {
      mal m = eval(ast.v->buf[i], e);
      RETURN_IF_EXCEPTION(m);
      to->buf[i] = m;
    }
    return mal_vec(to);
  }

  if (is_map(ast))
  {
    map *to = uninitialized_map(ast.m->size);
    for (int i = 0; i < ast.m->size; i++)
    {
      mal m = eval(ast.m->table[i].val, e);
      RETURN_IF_EXCEPTION(m);
      to->table[i].key = ast.m->table[i].key;
      to->table[i].is_kw = ast.m->table[i].is_kw;
      to->table[i].val = m;
    }
    return mal_map(to);
  }

  return ast;
}

// top-level evaluation function
mal eval(mal ast, env *e)
{
  DEBUG_HIGH_MAL("", ast);
  RETURN_IF_EXCEPTION(ast);

  if (!is_list(ast) || seq_empty(ast))
    return eval_ast(ast, e);

  // Check for special forms
  mal head = mal_first(ast);
  mal rest = mal_rest(ast);
  if (mal_equals(head, mal_sym("def!")))
    return def_special_form(rest.n, e);
  if (mal_equals(head, mal_sym("let*")))
    return let_special_form(rest.n, e);

  // Evaluate all the list elements
  mal evaluated_ast = eval_ast(ast, e);
  RETURN_IF_EXCEPTION(evaluated_ast);
  DEBUG_INTERNAL_MAL("after args evaluated have", evaluated_ast);

  // Apply the function
  head = mal_first(evaluated_ast);
  rest = mal_rest(evaluated_ast);
  DEBUG_INTERNAL_MAL("head of list to be applied", head);
  DEBUG_INTERNAL_MAL("rest of list to be applied", rest);
  // if (is_exception(head))
  //   return head;
  if (!is_fn(head))
    return mal_exception_str("Not a function");
  return head.f(rest.n, e);
}
