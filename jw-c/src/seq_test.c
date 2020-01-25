#include <stdio.h>
#include "minunit.h"

#include "seq_test.h"
#include "seq.h"

const char *seq_test() {

    // n0 = []
    list_node *n0 = NULL;
    mu_assert("n0 not empty", list_empty(n0));
    mu_assert("n0 not zero length", list_count(n0) == 0);

    // n1 = 1 : []
    list_node *n1 = list_cons(make_int(1), n0);
    mu_assert("n1 empty", !list_empty(n1));
    mu_assert("n1 not length one", list_count(n1) == 1);
    mu_assert("n1 head value wrong", mal_equals(n1->val, make_int(1)));
    mu_assert("n1 next not null", n1->next == NULL);

    // n2 = 2 : n1 = 2 : 1 : []
    list_node *n2 = list_cons(make_int(2), n1);
    mu_assert("n2 empty", !list_empty(n2));
    mu_assert("n2 not length two", list_count(n2) == 2);
    mu_assert("n2 head value wrong", mal_equals(n2->val, make_int(2)));
    mu_assert("n2 next not n1", n2->next == n1);
    mu_assert("n2 second value wrong", mal_equals(n2->next->val, make_int(1)));
    mu_assert("n2 second next not null", n2->next->next == NULL);

    // n3 = 3 : []
    list_node *n3 = list_extend(make_int(3), NULL);
    mu_assert("n3 empty", !list_empty(n3));
    mu_assert("n3 not length one", list_count(n3) == 1);
    mu_assert("n3 head value wrong", mal_equals(n3->val, make_int(3)));
    mu_assert("n3 next not null", n3->next == NULL);

    // n3 = 3 : n4 = 3 : 4 : []
    list_node *n4 = list_extend(make_int(4), n3);
    mu_assert("n4 empty", !list_empty(n4));
    mu_assert("n4 not length one", list_count(n4) == 1);
    mu_assert("n3 now not length two", list_count(n3) == 2);
    mu_assert("n4 head value wrong", mal_equals(n4->val, make_int(4)));
    mu_assert("n4 next not null", n4->next == NULL);
    mu_assert("n3 head value wrong", mal_equals(n3->val, make_int(3)));
    mu_assert("n3 next not n4", n3->next == n4);

    // Test of equality for lists
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

    // v0 = []
    vec *v0 = create_vec(0, n0);
    mu_assert("v0 not empty", vec_empty(v0));
    mu_assert("v0 not zero length", vec_count(v0) == 0);

    // v1 = [1]
    vec *v1 = create_vec(1, n1);
    mu_assert("v1 empty", !vec_empty(v1));
    mu_assert("v1 not length one", vec_count(v1) == 1);
    mu_assert("v1 value wrong", mal_equals(v1->buf[0], make_int(1)));

    // v2 = [2, 1]
    vec *v2 = create_vec(2, n2);
    mu_assert("v2 empty", !vec_empty(v2));
    mu_assert("v2 not length two", vec_count(v2) == 2);
    mu_assert("v2[0] value wrong", mal_equals(v2->buf[0], make_int(2)));
    mu_assert("v2[1] value wrong", mal_equals(v2->buf[1], make_int(1)));

    // v3 = [3]
    vec *v3 = create_vec(1, n3);
    mu_assert("v3 empty", !vec_empty(v3));
    mu_assert("v3 not length one", vec_count(v3) == 1);
    mu_assert("v3[0] value wrong", mal_equals(v3->buf[0], make_int(1)));

    // Test seq_equals for vectors and lists
    mal mn0 = make_list(n0);
    mal mn1 = make_list(n1);
    mal mn2 = make_list(n2);
    mal mn3 = make_list(n3);
    mal mv0 = make_vec(v0);
    mal mv1 = make_vec(v1);
    mal mv2 = make_vec(v2);
    mal mv3 = make_vec(v3);
    mu_assert("mn0 != mv0", mal_equals(mn0, mv0));
    mu_assert("mn1 != mv1", mal_equals(mn1, mv1));
    mu_assert("mn2 != mv2", mal_equals(mn2, mv2));
    mu_assert("mn3 != mv3", mal_equals(mn3, mv3));
    mu_assert("mv0 != mn0", mal_equals(mv0, mn0));
    mu_assert("mv1 != mn1", mal_equals(mv1, mn1));
    mu_assert("mv2 != mn2", mal_equals(mv2, mn2));
    mu_assert("mv3 != mn3", mal_equals(mv3, mn3));
    mu_assert("mn0 == mv1", mal_equals(mn0, mv1));
    mu_assert("mn1 == mv2", mal_equals(mn1, mv2));
    mu_assert("mn2 == mv3", mal_equals(mn2, mv3));
    mu_assert("mn3 == mv0", mal_equals(mn3, mv0));
    mu_assert("mv0 == mn1", mal_equals(mv0, mn1));
    mu_assert("mv1 == mn2", mal_equals(mv1, mn2));
    mu_assert("mv2 == mn3", mal_equals(mv2, mn3));
    mu_assert("mv3 == mn0", mal_equals(mv3, mn0));


    return 0;
}
