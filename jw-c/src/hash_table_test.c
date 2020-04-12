#include <stdio.h>

#include "hash_table.h"
#include "hash_table_test.h"
#include "minunit.h"
#include "seq.h"
#include "types.h"

const char *hash_table_test() {

  hash_table *h = ht_new(4);
  mal a = mal_int(2);
  mal b = mal_str("cat");
  mal c = mal_int(4);
  mal d = mal_str("house");
  mal e = mal_int(7);
  ht_put(h, "a", a);
  ht_put(h, "b", b);
  mu_assert_eq("get a", ht_get(h, "a"), a);
  mu_assert_eq("get b", ht_get(h, "b"), b);
  mu_assert("has a", ht_has(h, "a"));
  mu_assert("has b", ht_has(h, "b"));
  mu_assert("has c", !ht_has(h, "c"));

  // Trigger resizing
  ht_put(h, "c", c);
  ht_put(h, "d", d);
  ht_put(h, "e", e);
  ht_put(h, "c1", c);
  ht_put(h, "d1", d);
  ht_put(h, "e1", e);
  ht_put(h, "c2", c);
  ht_put(h, "d2", d);
  ht_put(h, "e2", e);

  mu_assert_eq("get a", ht_get(h, "a"), a);
  mu_assert_eq("get b", ht_get(h, "b"), b);
  mu_assert_eq("get c", ht_get(h, "c"), c);
  mu_assert_eq("get d", ht_get(h, "d"), d);
  mu_assert_eq("get e", ht_get(h, "e"), e);
  mu_assert("has a", ht_has(h, "a"));
  mu_assert("has b", ht_has(h, "b"));
  mu_assert("has c", ht_has(h, "c"));
  mu_assert("has e", ht_has(h, "d"));
  mu_assert("has e", ht_has(h, "e"));
  mu_assert("has f", !ht_has(h, "f"));

  mal v1[] = {mal_str("a"), a, mal_str("b"), b};
  list_node *n1 = array_to_list(4, v1);
  hash_table *h1 = ht_from_alternating_list(n1);
  mu_assert_eq("get a", ht_get(h1, "a"), a);
  mu_assert_eq("get b", ht_get(h1, "b"), b);

  mal v2a[] = {mal_str("a"), mal_str("c")};
  mal v2b[] = {a, c};
  list_node *n2a = array_to_list(2, v2a);
  list_node *n2b = array_to_list(2, v2b);
  hash_table *h2 = ht_from_lists(n2a, n2b);
  mu_assert_eq("get a", ht_get(h2, "a"), a);
  mu_assert_eq("get c", ht_get(h2, "c"), c);

  hash_table *h3 = ht_new(1);
  ht_put(h3, "abc", mal_int(2));
  mu_assert_eq("keys h3", mal_list(ht_keys(h3)),
               mal_cons(mal_str("abc"), mal_list(NULL)));
  mu_assert_eq("values h3", mal_list(ht_values(h3)),
               mal_cons(mal_int(2), mal_list(NULL)));

  return 0;
}
