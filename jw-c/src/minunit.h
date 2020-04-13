/* file: minunit.h */

/**
 *
 * Minunit - Minimal unit testing framework for C
 *
 * http://www.jera.com/techinfo/jtns/jtn002.html
 *
 */

#include "eval.h"
#include "reader.h"

#define mu_run_test(test)                                                      \
  do {                                                                         \
    const char *message = test();                                              \
    tests_run++;                                                               \
    if (message)                                                               \
      return message;                                                          \
  } while (0)

#define mu_assert(message, test)                                               \
  do {                                                                         \
    asserts_run++;                                                             \
    if (!(test))                                                               \
      return __FILE__ ": " message;                                            \
  } while (0)

// convenience versions
#define mu_assert_str(message, s1, s2) mu_assert(message, strcmp(s1, s2) == 0)
#define mu_assert_eq(message, m1, m2) mu_assert(message, mal_equals(m1, m2))
#define mu_assert_neq(message, m1, m2) mu_assert(message, !mal_equals(m1, m2))

// convemience version with read/eval
#define mu_assert_mal(e, code, val)                                            \
  mu_assert(code, mal_equals(eval(read_str(code), e), val))
#define mu_assert_exception(e, code)                                           \
  mu_assert(code, is_exception(eval(read_str(code), e)))
#define mu_assert_mal2(e, code1, code2)                                        \
  mu_assert(code1 "/" code2,                                                   \
            mal_equals(eval(read_str(code1), e), eval(read_str(code2), e)))

// macro to read and evaluate an expression for use in tests
#define E(expr, e) eval(read_str(expr), e)

extern int asserts_run;
extern int tests_run;
