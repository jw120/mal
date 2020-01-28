#include <stdio.h>

#include "minunit.h"
#include "map_test.h"
#include "map.h"

#include "seq.h"

#include "printer.h"

const char *map_test() {

    // map with strings
    map *m = list_to_map(
        list_cons(mal_str("cat"), list_cons(mal_int(3),
            list_cons(mal_str("dog"), list_cons(mal_int(1), NULL)))));
    mu_assert_eq("map get cat", map_get(m, mal_str("cat")), mal_int(3));
    mu_assert_eq("map get dog", map_get(m, mal_str("dog")), mal_int(1));
    mu_assert_eq("map get xxx", map_get(m, mal_str("xxx")), mal_nil());
    mu_assert("map contains cat", map_contains(m, mal_str("cat")));
    mu_assert("map contains dog", map_contains(m, mal_str("dog")));
    mu_assert("map contains xxx", !map_contains(m, mal_str("xxx")));

    // map with strings and keywords
    m = list_to_map(
        list_cons(mal_str("cat"), list_cons(mal_int(3),
        list_cons(mal_kw("cat"), list_cons(mal_int(5),
        list_cons(mal_str("dog"), list_cons(mal_int(1), NULL)))))));
    mu_assert_eq("map-kw get cat", map_get(m, mal_str("cat")), mal_int(3));
    mu_assert_eq("map-kw get :cat", map_get(m, mal_kw("cat")), mal_int(5));
    mu_assert_eq("map-kw get dog", map_get(m, mal_str("dog")), mal_int(1));

    // map with duplicates
    m = list_to_map(
        list_cons(mal_str("cat"), list_cons(mal_int(3),
        list_cons(mal_str("cat"), list_cons(mal_int(0),
        list_cons(mal_str("dog"), list_cons(mal_int(1),
        list_cons(mal_str("cat"), list_cons(mal_int(1),
        list_cons(mal_str("dog"), list_cons(mal_int(7), NULL)))))))))));
    mu_assert_eq("map-dup get cat", map_get(m, mal_str("cat")), mal_int(1));
    mu_assert_eq("map-dup get dog", map_get(m, mal_str("dog")), mal_int(7));

    // map equality
    mal m1_list = mal_cons(mal_int(1), mal_list(NULL));
    mal m1 = mal_map(list_to_map(
            list_cons(mal_kw("cat"),
            list_cons(m1_list,
            list_cons(mal_kw("dog"),
            list_cons(mal_int(2), NULL))))));
    mal m2_vec = mal_vec(list_to_vec(1, list_cons(mal_int(1), NULL)));
    mal m2 = mal_map(list_to_map(
            list_cons(mal_kw("dog"),
            list_cons(mal_int(2),
            list_cons(mal_kw("cat"),
            list_cons(m2_vec, NULL))))));
    mu_assert_eq("map_equality m1==m2", m1, m2);
    mu_assert_neq("map_equality m1/=m", m1, mal_map(m));

    return 0;

}
