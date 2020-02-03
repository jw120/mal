#include <stdio.h>

#include "map.h"
#include "map_test.h"
#include "minunit.h"

#include "seq.h"

#include "printer.h"

const char *map_test()
{

  // map with strings
  map *m = list_to_map(
      list_cons(mal_str("cat"),
                list_cons(mal_int(3), list_cons(mal_str("dog"),
                                                list_cons(mal_int(1), NULL)))));
  mu_assert_eq("map get cat", map_get(m, mal_str("cat")), mal_int(3));
  mu_assert_eq("map get dog", map_get(m, mal_str("dog")), mal_int(1));
  mu_assert_eq("map get xxx", map_get(m, mal_str("xxx")), mal_nil());
  mu_assert("map contains cat", map_contains(m, mal_str("cat")));
  mu_assert("map contains dog", map_contains(m, mal_str("dog")));
  mu_assert("map contains xxx", !map_contains(m, mal_str("xxx")));

  // map with strings and keywords
  m = list_to_map(list_cons(
      mal_str("cat"),
      list_cons(mal_int(3),
                list_cons(mal_kw("cat"),
                          list_cons(mal_int(5),
                                    list_cons(mal_str("dog"),
                                              list_cons(mal_int(1), NULL)))))));
  mu_assert_eq("map-kw get cat", map_get(m, mal_str("cat")), mal_int(3));
  mu_assert_eq("map-kw get :cat", map_get(m, mal_kw("cat")), mal_int(5));
  mu_assert_eq("map-kw get dog", map_get(m, mal_str("dog")), mal_int(1));

  // map with duplicates
  m = list_to_map(list_cons(
      mal_str("cat"),
      list_cons(
          mal_int(3),
          list_cons(
              mal_str("cat"),
              list_cons(
                  mal_int(0),
                  list_cons(
                      mal_str("dog"),
                      list_cons(
                          mal_int(1),
                          list_cons(
                              mal_str("cat"),
                              list_cons(mal_int(1),
                                        list_cons(mal_str("dog"),
                                                  list_cons(mal_int(7),
                                                            NULL)))))))))));
  mu_assert_eq("map-dup get cat", map_get(m, mal_str("cat")), mal_int(1));
  mu_assert_eq("map-dup get dog", map_get(m, mal_str("dog")), mal_int(7));

  // map equality
  mal m1_list = mal_cons(mal_int(1), mal_list(NULL));
  mal m1 = mal_map(list_to_map(
      list_cons(mal_kw("cat"),
                list_cons(m1_list, list_cons(mal_kw("dog"),
                                             list_cons(mal_int(2), NULL))))));
  mal m2_vec = mal_vec(list_to_vec(1, list_cons(mal_int(1), NULL)));
  mal m2 = mal_map(list_to_map(
      list_cons(mal_kw("dog"),
                list_cons(mal_int(2),
                          list_cons(mal_kw("cat"), list_cons(m2_vec, NULL))))));
  mu_assert_eq("map_equality m1==m2", m1, m2);
  mu_assert_neq("map_equality m1/=m", m1, mal_map(m));

  // map setting
  mal a[] = {mal_kw("a"), mal_int(7), mal_kw("b"), mal_int(4)};
  m = list_to_map(array_to_list(4, a));
  map_set(m, mal_kw("b"), mal_int(6));
  mu_assert_eq("map set existing", map_get(m, mal_kw("b")), mal_int(6));
  map_set(m, mal_kw("c"), mal_int(2));
  mu_assert_eq("map set new", map_get(m, mal_kw("c")), mal_int(2));

  // list2_to_map
  list_node *binds = list_cons(mal_sym("p"), list_cons(mal_sym("q"), NULL));
  list_node *vals = list_cons(mal_int(7), list_cons(mal_int(6), NULL));
  m = list2_to_map(binds, vals);
  mu_assert_eq("m list2 y", map_get(m, mal_sym("y")), mal_nil());
  mu_assert_eq("m list2 p", map_get(m, mal_sym("p")), mal_int(7));
  mu_assert_eq("m list2 q", map_get(m, mal_sym("q")), mal_int(6));

  // list2_to_map with &
  binds = list_cons(mal_sym("p"), list_cons(mal_sym("&"), list_cons(mal_sym("q"), NULL)));
  vals = list_cons(mal_int(7), list_cons(mal_int(6), list_cons(mal_int(5), list_cons(mal_int(4), NULL))));
  m = list2_to_map(binds, vals);
  mu_assert_eq("m &list2 y", map_get(m, mal_sym("y")), mal_nil());
  mu_assert_eq("m &list2 p", map_get(m, mal_sym("p")), mal_int(7));
  mu_assert_eq("m &list2 q", map_get(m, mal_sym("q")), mal_list(list_cons(mal_int(6), list_cons(mal_int(5), list_cons(mal_int(4), NULL)))));

  // list2_to_map with & and empty following list
  binds = list_cons(mal_sym("p"), list_cons(mal_sym("&"), list_cons(mal_sym("q"), NULL)));
  vals = list_cons(mal_int(7), NULL);
  m = list2_to_map(binds, vals);
  mu_assert_eq("m &list2 p", map_get(m, mal_sym("p")), mal_int(7));
  mu_assert_eq("m &list2 q", map_get(m, mal_sym("q")), mal_list(NULL));

  return 0;
}
