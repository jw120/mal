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
  if (!is_sym(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to def!");
  mal evaluated_val = eval(n->next->val, e);
  RETURN_IF_EXCEPTION(evaluated_val);
  env_set(e, n->val.s, evaluated_val);
  return evaluated_val;
}

mal let_special_form(list_node *n, env *e)
{
  return mal_nil();
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
  if (mal_equals(head, mal_sym("def!")))
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
