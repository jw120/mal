/**
 *
 * eval.c - provides the evaluator
 *
 **/

#include "eval.h"

#include "debug.h"
#include "map.h"
#include "printer.h"
#include "seq.h"
#include "utils.h"

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
  DEBUG_HIGH_MAL("called with", ast);

  if (!is_list(ast) || seq_empty(ast))
    return eval_ast(ast, e);

  mal evaluated_ast = eval_ast(ast, e);
  DEBUG_INTERNAL_MAL("after args evaluated have", evaluated_ast);
  if (!is_list(evaluated_ast) || seq_empty(evaluated_ast))
    return mal_exception_str("List mutatated away in eval");

  mal head = mal_first(evaluated_ast);
  mal rest = mal_rest(evaluated_ast);
  DEBUG_INTERNAL_MAL("head of list to be applied", head);
  DEBUG_INTERNAL_MAL("rest of list to be applied", rest);

  if (!is_fn(head))
    return mal_exception_str("Not a function");
  return head.f(rest.n, e);
}
