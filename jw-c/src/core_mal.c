/**
 *
 * core_miscc - defines misc functions for the core environment
 *
 **/

#include <assert.h>

#include "core_misc.h"

#include "eval.h"
#include "reader.h"

static char *s1 = "(def! not"
                  "  (fn* (a)"
                  "    (if a false true)))";

static char *s2 = "(def! load-file"
                  "  (fn* (f)"
                  "    (eval (read-string"
                  "    (str \"(do \" (slurp f) \"\\nnil) \")))))";

static char *s3 = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) "
                  "(list 'if (first xs) (if (> (count xs) 1) (nth xs 1) "
                  "(throw \"odd number of forms to cond\")) (cons 'cond (rest "
                  "(rest xs)))))))";

void add_mal(env *e) {
  assert(!is_exception(eval(read_str(s1), e)));
  assert(!is_exception(eval(read_str(s2), e)));
  assert(!is_exception(eval(read_str(s3), e)));
}
