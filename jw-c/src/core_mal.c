/**
 *
 * core_miscc - defines misc functions for the core environment
 *
 **/

#include <assert.h>

#include "core_misc.h"

#include "eval.h"
#include "reader.h"

// add mal-defined functions to the environment
void add_mal(env *e)
{
  mal ret = eval(read_str("(def! not (fn* (a) (if a false true)))"), e);
  assert(!is_exception(ret));
}
