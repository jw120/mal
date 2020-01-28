#include <stdio.h>

#include "minunit.h"
#include "map_test.h"
#include "map.h"

#include "seq.h"

#include "printer.h"

const char *map_test() {

    map *m = list_to_map(
        list_cons(mal_str("cat"), list_cons(mal_int(3),
            list_cons(mal_str("dog"), list_cons(mal_int(1), NULL)))));
    puts(pr_str(mal_map(m), true));
    mu_assert_eq("map get 1", map_get(m, mal_str("cat")), mal_int(3));
    mu_assert_eq("map get 2", map_get(m, mal_str("dog")), mal_int(1));
    mu_assert_eq("map get 3", map_get(m, mal_str("xxx")), mal_nil());
    mu_assert("map contains 1", map_contains(m, mal_str("cat")));
    mu_assert("map contains 2", map_contains(m, mal_str("dog")));
    mu_assert("map contains 3", !map_contains(m, mal_str("xxx")));

    return 0;

}
