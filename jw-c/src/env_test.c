#include <stdio.h>

#include "env.h"
#include "env_test.h"
#include "minunit.h"

#include "seq.h"

const char *env_test()
{

  // create three environments
  // e0 is empty with no outer
  // e1 defines (a, 3) and (b, 4) with no outer
  // e2 defines (a, 2) with e1 as the outer
  mal a1[] = {mal_sym("a"), mal_int(3), mal_sym("b"), mal_int(4)};
  mal a2[] = {mal_sym("a"), mal_int(2)};
  env *e0 = env_new(NULL, NULL);
  env *e1 = env_new(array_to_list(4, a1), NULL);
  env *e2 = env_new(array_to_list(2, a2), e1);

  // test env_find
  mu_assert("e0 find a", env_find(e0, "a") == NULL);
  mu_assert("e1 find a", env_find(e1, "a") == e1);
  mu_assert("e1 find b", env_find(e1, "b") == e1);
  mu_assert("e1 find c", env_find(e1, "c") == NULL);
  mu_assert("e2 find a", env_find(e2, "a") == e2);
  mu_assert("e2 find b", env_find(e2, "b") == e1);
  mu_assert("e2 find c", env_find(e2, "c") == NULL);

  // test env_get
  mu_assert("e0 get a", is_exception(env_get(e0, "a")));
  mu_assert_eq("e1 has a", env_get(e1, "a"), mal_int(3));
  mu_assert_eq("e1 has b", env_get(e1, "b"), mal_int(4));
  mu_assert("e1 get c", is_exception(env_get(e1, "c")));
  mu_assert_eq("e2 has a", env_get(e2, "a"), mal_int(2));
  mu_assert_eq("e2 has b", env_get(e2, "b"), mal_int(4));
  mu_assert("e2 get c", is_exception(env_get(e2, "c")));

  // test env_set
  env_set(e0, "x", mal_int(31));
  env_set(e1, "x", mal_int(32));
  env_set(e2, "x", mal_int(33));
  env_set(e2, "a", mal_int(4));
  mu_assert_eq("e0 new set", env_get(e0, "x"), mal_int(31));
  mu_assert_eq("e1 new set", env_get(e1, "x"), mal_int(32));
  mu_assert_eq("e2 new set", env_get(e2, "x"), mal_int(33));
  mu_assert_eq("e1 remains", env_get(e1, "a"), mal_int(3));
  mu_assert_eq("e2 updated", env_get(e2, "a"), mal_int(4));

  return 0;
}
