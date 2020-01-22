#include <stdio.h>
#include "minunit.h"

#include "test_list.h"

int tests_run = 0;

const char *run_tests() {
     mu_run_test(test_list);
     return 0;
 }

int main(int argc, char **argv) {

    printf("Starting tests\n");

    char *result = run_tests();

    if (result != 0) {
        printf("%s\n", result);
    } else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}