#ifndef EVAL_H
#define EVAL_H

#include "env.h"
#include "types.h"

mal eval_ast(mal, env *);
mal eval(mal, env *);

#endif