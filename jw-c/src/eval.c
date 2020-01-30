/**
 *
 * eval.c - provides the evaluator
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

mal def_special_form(list_node *n, env *e)
{
  DEBUG_INTERNAL_MAL("", mal_list(n));
  if (!is_sym(n->val) || list_count(n) != 2)
    return mal_exception_str("Bad arguments to def!");
  mal evaluated_val = eval(n->next->val, e);
  if (!is_exception(evaluated_val))
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
      to = list_extend(eval(from->val, e), to);
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
      to->buf[i] = eval(ast.v->buf[i], e);
    return mal_vec(to);
  }

  if (is_map(ast))
  {
    map *to = uninitialized_map(ast.m->size);
    for (int i = 0; i < ast.m->size; i++)
    {
      to->table[i].key = ast.m->table[i].key;
      to->table[i].is_kw = ast.m->table[i].is_kw;
      to->table[i].val = eval(ast.m->table[i].val, e);
    }
    return mal_map(to);
  }

  return ast;
}

// top-level evaluation function
mal eval(mal ast, env *e)
{
  DEBUG_HIGH_MAL("", ast);

  if (is_exception(ast))
    return ast;

  if (!is_list(ast) || seq_empty(ast))
    return eval_ast(ast, e);

  // Check for special forms
  mal head = mal_first(ast);
  mal rest = mal_rest(ast);
  assert(is_list(rest));
  if (mal_equals(head, mal_sym("def!")))
    return def_special_form(rest.n, e);
  if (mal_equals(head, mal_sym("def!")))
    return let_special_form(rest.n, e);

  mal evaluated_ast = eval_ast(ast, e);
  DEBUG_INTERNAL_MAL("after args evaluated have", evaluated_ast);
  if (!is_list(evaluated_ast) || seq_empty(evaluated_ast))
    return mal_exception_str("List mutatated away in eval");

  head = mal_first(evaluated_ast);
  rest = mal_rest(evaluated_ast);
  assert(is_list(rest));
  DEBUG_INTERNAL_MAL("head of list to be applied", head);
  DEBUG_INTERNAL_MAL("rest of list to be applied", rest);

  // If not a special form, apply the function
  if (is_exception(head))
    return head;
  if (!is_fn(head))
    return mal_exception_str("Not a function");
  return head.f(rest.n, e);
}
