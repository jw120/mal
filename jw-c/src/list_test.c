#include <stdio.h>
#include "minunit.h"

#include "list_test.h"
#include "list.h"

const char *list_test() {

    // n0 = []
    list_node *n0 = NULL;
    mu_assert("n0 not empty", list_empty(n0));
    mu_assert("n0 not zero length", list_len(n0) == 0);

    // n1 = 1 : []
    list_node *n1 = list_cons(make_int(1), n0);
    mu_assert("n1 empty", !list_empty(n1));
    mu_assert("n1 not length one", list_len(n1) == 1);
    mu_assert("n1 head value wrong", mal_equals(n1->val, make_int(1)));
    mu_assert("n1 next not null", n1->next == NULL);

    // n2 = 2 : n1 = 2 : 1 : []
    list_node *n2 = list_cons(make_int(2), n1);
    mu_assert("n2 empty", !list_empty(n2));
    mu_assert("n2 not length two", list_len(n2) == 2);
    mu_assert("n2 head value wrong", mal_equals(n2->val, make_int(2)));
    mu_assert("n2 next not n1", n2->next == n1);
    mu_assert("n2 second value wrong", mal_equals(n2->next->val, make_int(1)));
    mu_assert("n2 second next not null", n2->next->next == NULL);

    // n3 = 3 : []
    list_node *n3 = list_extend(make_int(3), NULL);
    mu_assert("n3 empty", !list_empty(n3));
    mu_assert("n3 not length one", list_len(n3) == 1);
    mu_assert("n3 head value wrong", mal_equals(n3->val, make_int(3)));
    mu_assert("n3 next not null", n3->next == NULL);

    // n3 = 3 : n4 = 3 : 4 : []
    list_node *n4 = list_extend(make_int(4), n3);
    mu_assert("n4 empty", !list_empty(n4));
    mu_assert("n4 not length one", list_len(n4) == 1);
    mu_assert("n3 now not length two", list_len(n3) == 2);
    mu_assert("n4 head value wrong", mal_equals(n4->val, make_int(4)));
    mu_assert("n4 next not null", n4->next == NULL);
    mu_assert("n3 head value wrong", mal_equals(n3->val, make_int(3)));
    mu_assert("n3 next not n4", n3->next == n4);

    // Test of equality
    mu_assert("n0 != n0", list_equals(n0, n0));
    mu_assert("n0 == n1", !list_equals(n0, n1));
    mu_assert("n0 == n2", !list_equals(n0, n2));
    mu_assert("n0 == n3", !list_equals(n0, n3));
    mu_assert("n0 == n4", !list_equals(n0, n4));
    mu_assert("n1 == n0", !list_equals(n1, n0));
    mu_assert("n1 /= n1", list_equals(n1, n1));
    mu_assert("n1 == n2", !list_equals(n1, n2));
    mu_assert("n1 == n3", !list_equals(n1, n3));
    mu_assert("n1 == n4", !list_equals(n1, n4));
    mu_assert("n2 == n0", !list_equals(n2, n0));
    mu_assert("n2 == n1", !list_equals(n2, n1));
    mu_assert("n2 /= n2", list_equals(n2, n2));
    mu_assert("n2 == n3", !list_equals(n2, n3));
    mu_assert("n2 == n4", !list_equals(n2, n4));
    mu_assert("n3 == n0", !list_equals(n3, n0));
    mu_assert("n3 == n1", !list_equals(n3, n1));
    mu_assert("n3 == n2", !list_equals(n3, n2));
    mu_assert("n3 /= n3", list_equals(n3, n3));
    mu_assert("n3 == n4", !list_equals(n3, n4));
    mu_assert("n4 == n0", !list_equals(n4, n0));
    mu_assert("n4 == n1", !list_equals(n4, n1));
    mu_assert("n4 == n2", !list_equals(n4, n2));
    mu_assert("n4 == n3", !list_equals(n4, n3));
    mu_assert("n4 /= n4", list_equals(n4, n4));

    return 0;
}
