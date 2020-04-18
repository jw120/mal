#ifndef EVAL_H
#define EVAL_H

#include "env.h"
#include "types.h"

// Full evaluation of the given ast in the environment
mal eval(mal, env *);

// Evaluate the ast in the environment without the apply phase (i.e., not
// applying functions or special forms)
mal eval_ast(mal, env *);

// Evalulate a function or closure with the given list of arguments
mal apply(mal, list_node *, env *);

#endif
