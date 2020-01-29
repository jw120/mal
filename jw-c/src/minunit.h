/* file: minunit.h */

/**
 *
 * Minunit - Minimal unit testing framework for C
 *
 * http://www.jera.com/techinfo/jtns/jtn002.html
 *
 */

#define mu_assert(message, test)                                               \
  do {                                                                         \
    asserts_run++;                                                             \
    if (!(test))                                                               \
      return message;                                                          \
  } while (0)
#define mu_run_test(test)                                                      \
  do {                                                                         \
    const char *message = test();                                              \
    tests_run++;                                                               \
    if (message)                                                               \
      return message;                                                          \
  } while (0)

// convenience versions
#define mu_assert_str(message, s1, s2)                                         \
  do {                                                                         \
    asserts_run++;                                                             \
    if (strcmp(s1, s2))                                                        \
      return message;                                                          \
  } while (0)
#define mu_assert_eq(message, m1, m2)                                          \
  do {                                                                         \
    asserts_run++;                                                             \
    if (!mal_equals(m1, m2))                                                   \
      return message;                                                          \
  } while (0)
#define mu_assert_neq(message, m1, m2)                                         \
  do {                                                                         \
    asserts_run++;                                                             \
    if (mal_equals(m1, m2))                                                    \
      return message;                                                          \
  } while (0)

extern int asserts_run;
extern int tests_run;
