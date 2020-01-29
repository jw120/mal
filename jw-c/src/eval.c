/**
 *
 * eval.c - provides the evaluator
 *
 **/

#include "eval.h"

#include "printer.h"
#include "seq.h"
#include "utils.h"

// evaluation function that does not apply
mal eval_ast(mal ast, env *e)
{
  debug("eval_ast", "%s", pr_str(ast, true));

  if (is_sym(ast))
  {
    debug("eval_ast", "looking up %s in %p", ast.s, e);
    return env_get(e, ast.s);
  }

  if (is_list(ast) && !seq_empty(ast))
  {
    list_node *to_head = NULL;
    list_node *to = to_head;
    list_node *from = ast.n;
    while (from != NULL)
    {
      to = list_extend(eval_ast(from->val, e), to);
      if (to_head == NULL)
        to_head = to;
      from = from->next;
    }
    return mal_list(to_head);
  }

  return ast;
}

// top-level evaluation function
mal eval(mal ast, env *e)
{
  debug("eval", "%s", pr_str(ast, true));

  if (!is_list(ast) || seq_empty(ast))
    return eval_ast(ast, e);

  mal evaluated_ast = eval_ast(ast, e);
  if (!is_list(evaluated_ast) || seq_empty(evaluated_ast))
    return mal_exception(mal_str("List mutatated away in eval"));

  mal head = mal_first(evaluated_ast);
  mal rest = mal_rest(evaluated_ast);
  debug("eval", "inside ast");
  debug("eval", "ast %s, head %s, rest %s", pr_str(evaluated_ast, true), pr_str(head, true), pr_str(rest, true));
  if (!is_fn(head))
    return mal_exception(mal_str("Not a function"));
  return head.f(rest.n, e);
}
