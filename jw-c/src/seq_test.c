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
    list_node *n1 = list_cons(mal_int(1), n0);
    mu_assert("n1 empty", !list_empty(n1));
    mu_assert("n1 not length one", list_count(n1) == 1);
    mu_assert_eq("n1 head value wrong", n1->val, mal_int(1));
    mu_assert("n1 next not null", n1->next == NULL);

    // n2 = 2 : n1 = 2 : 1 : []
    list_node *n2 = list_cons(mal_int(2), n1);
    mu_assert("n2 empty", !list_empty(n2));
    mu_assert("n2 not length two", list_count(n2) == 2);
    mu_assert_eq("n2 head value wrong", n2->val, mal_int(2));
    mu_assert("n2 next not n1", n2->next == n1);
    mu_assert_eq("n2 second value wrong", n2->next->val, mal_int(1));
    mu_assert("n2 second next not null", n2->next->next == NULL);

    // n3 = 3 : []
    list_node *n3 = list_extend(mal_int(3), NULL);
    mu_assert("n3 empty", !list_empty(n3));
    mu_assert("n3 not length one", list_count(n3) == 1);
    mu_assert_eq("n3 head value wrong", n3->val, mal_int(3));
    mu_assert("n3 next not null", n3->next == NULL);

    // n3 = 3 : n4 = 3 : 4 : []
    list_node *n4 = list_extend(mal_int(4), n3);
    mu_assert("n4 empty", !list_empty(n4));
    mu_assert("n4 not length one", list_count(n4) == 1);
    mu_assert("n3 now not length two", list_count(n3) == 2);
    mu_assert_eq("n4 head value wrong", n4->val, mal_int(4));
    mu_assert("n4 next not null", n4->next == NULL);
    mu_assert_eq("n3 head value wrong", n3->val, mal_int(3));
    mu_assert("n3 next not n4", n3->next == n4);

    // mal_cons, mal_tail and mal_head on lists
    mal x0 = mal_list(NULL);
    mal x1 = mal_cons(mal_int(3), x0);
    mal x2 = mal_cons(mal_sym("ab"), x1);
    mu_assert("mal_cons length", seq_count(x2) == 2);
    mu_assert_eq("mal_cons first", mal_first(x2), mal_sym("ab"));
    mu_assert_eq("mal_cons first rest", mal_first(mal_rest(x2)), mal_int(3));
    mu_assert_eq("mal_head nil", mal_first(mal_nil()), mal_nil());
    mu_assert_eq("mal_head ()", mal_first(mal_list(NULL)), mal_nil());
    mu_assert_eq("mal_tail nil", mal_rest(mal_nil()), mal_list(NULL));
    mu_assert_eq("mal_tail ()", mal_rest(mal_list(NULL)), mal_list(NULL));

   // mal_cons, mal_tail and mal_head on lists
    mal xv = mal_vec(list_to_vec(2, x2.n));
    mal xv0 = mal_vec(list_to_vec(0, NULL));
    mu_assert("mal_cons vec length", seq_count(xv) == 2);
    mu_assert_eq("mal_cons vec first", mal_first(xv), mal_sym("ab"));
    mu_assert_eq("mal_cons vec first rest", mal_first(mal_rest(xv)), mal_int(3));
    mu_assert_eq("mal_head vec []", mal_first(xv0), mal_nil());
    mu_assert_eq("mal_tail vec []", mal_rest(xv0), mal_list(NULL));

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
    vec *v0 = list_to_vec(0, n0);
    mu_assert("v0 not empty", vec_empty(v0));
    mu_assert("v0 not zero length", vec_count(v0) == 0);

    // v1 = [1]
    vec *v1 = list_to_vec(1, n1);
    mu_assert("v1 empty", !vec_empty(v1));
    mu_assert("v1 not length one", vec_count(v1) == 1);
    mu_assert_eq("v1 value wrong", v1->buf[0], mal_int(1));

    // v2 = [2, 1]
    vec *v2 = list_to_vec(2, n2);
    mu_assert("v2 empty", !vec_empty(v2));
    mu_assert("v2 not length two", vec_count(v2) == 2);
    mu_assert_eq("v2[0] value wrong", v2->buf[0], mal_int(2));
    mu_assert_eq("v2[1] value wrong", v2->buf[1], mal_int(1));

    // v3 = [3, 4]
    vec *v3 = list_to_vec(2, n3);
    mu_assert("v3 empty", !vec_empty(v3));
    mu_assert("v3 not length two", vec_count(v3) == 2);
    mu_assert_eq("v3[0] value wrong", v3->buf[0], mal_int(3));
    mu_assert_eq("v3[1] value wrong", v3->buf[1], mal_int(4));

    // Test seq_equals for vectors and lists
    mal mn0 = mal_list(n0);
    mal mn1 = mal_list(n1);
    mal mn2 = mal_list(n2);
    mal mn3 = mal_list(n3);
    mal mv0 = mal_vec(v0);
    mal mv1 = mal_vec(v1);
    mal mv2 = mal_vec(v2);
    mal mv3 = mal_vec(v3);
    mu_assert_eq("mn0 != mv0", mn0, mv0);
    mu_assert_eq("mn1 != mv1", mn1, mv1);
    mu_assert_eq("mn2 != mv2", mn2, mv2);
    mu_assert_eq("mn3 != mv3", mn3, mv3);
    mu_assert_eq("mv0 != mn0", mv0, mn0);
    mu_assert_eq("mv1 != mn1", mv1, mn1);
    mu_assert_eq("mv2 != mn2", mv2, mn2);
    mu_assert_eq("mv3 != mn3", mv3, mn3);
    mu_assert_neq("mn0 == mv1", mn0, mv1);
    mu_assert_neq("mn1 == mv2", mn1, mv2);
    mu_assert_neq("mn2 == mv3", mn2, mv3);
    mu_assert_neq("mn3 == mv0", mn3, mv0);
    mu_assert_neq("mv0 == mn1", mv0, mn1);
    mu_assert_neq("mv1 == mn2", mv1, mn2);
    mu_assert_neq("mv2 == mn3", mv2, mn3);
    mu_assert_neq("mv3 == mn0", mv3, mn0);

    return 0;
}
