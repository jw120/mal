/* file: minunit.h */

/**
 *
 * Minunit - Minimal unit testing framework for C
 *
 * http://www.jera.com/techinfo/jtns/jtn002.html
 *
 */

#include <stdio.h>

#include "eval.h"
#include "reader.h"

// Basic minunit macro to run a test function (which is a series of assertions)
#define mu_run_test_function(test)                                             \
  do {                                                                         \
    const char *message = test();                                              \
    tests_run++;                                                               \
    if (message)                                                               \
      return message;                                                          \
  } while (0)

// Basic macro to make an assertion (test fails if the assertion is not true)
#define mu_assert(message, test)                                               \
  do {                                                                         \
    asserts_run++;                                                             \
    if (!(test))                                                               \
      return __FILE__ ": " message;                                            \
  } while (0)

// Convenience version of my_assert that tests if two strings are equal
#define mu_assert_str(message, s1, s2) mu_assert(message, strcmp(s1, s2) == 0)

// Convenience version of my_assert that tests if two mal values are equal
// (using mal_equals)
#define mu_assert_eq(message, m1, m2) mu_assert(message, mal_equals(m1, m2))

// Convenience version of my_assert that tests if two mal values are not equal
// (using mal_equals)
#define mu_assert_neq(message, m1, m2) mu_assert(message, !mal_equals(m1, m2))

// Convenience version of my_assert reads and evaluates a string as mal
// and compares it (with mal_equals) to the given mal value
#define mu_assert_mal(e, code, val)                                            \
  mu_assert(code, mal_equals(eval(read_str(code), e), val))

// Convenience version of my_assert reads and evaluates a string as mal
// and passes the test if is is an exception
#define mu_assert_exception(e, code)                                           \
  mu_assert(code, is_exception(eval(read_str(code), e)))

// Convenience version of my_assert reads and evaluates two strings as mal
// and compares ithen wiht (with mal_equals)
#define mu_assert_mal2(e, code1, code2)                                        \
  mu_assert(code1 "/" code2,                                                   \
            mal_equals(eval(read_str(code1), e), eval(read_str(code2), e)))

// Helper macro to read and evaluate an expression for use in tests
#define E(expr, e) eval(read_str(expr), e)

extern int tests_run;   // used in mu_test_function
extern int asserts_run; // used in mu_assert
