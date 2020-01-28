#include <stdio.h>

#include "minunit.h"
#include "map_test.h"
#include "map.h"

#include "seq.h"

const char *map_test() {

    map *m = list_to_map(
        list_cons(mal_str("cat"), list_cons(mal_int(3),
            list_cons(mal_str("dog"), list_cons(mal_int(1), NULL)))));
    mu_assert_eq("map get 1", map_get(m, "cat"), mal_int(3));
    mu_assert_eq("map get 2", map_get(m, "dog"), mal_int(1));
    mu_assert_eq("map get 3", map_get(m, "xxx"), mal_nil());
    mu_assert("map contains 1", map_contains(m, "cat"));
    mu_assert("map contains 2", map_contains(m, "dog"));
    mu_assert("map contains 3", !map_contains(m, "xxx"));


    return 0;

}
