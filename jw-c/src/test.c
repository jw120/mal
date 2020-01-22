#include <stdio.h>
#include "minunit.h"

#include "list_test.h"
#include "reader_test.h"
#include "tokenize_test.h"

int asserts_run = 0;
int tests_run = 0;

const char *run_tests() {
    mu_run_test(list_test);
    mu_run_test(reader_test);
    mu_run_test(tokenize_test);
    return 0;
 }

int main(int argc, char **argv) {

    printf("Starting tests\n");

    const char *result = run_tests();

    if (result != 0) {
        printf("%s\n", result);
    } else {
        printf("ALL TESTS PASSED\n");
    }
    printf("Tested %d assertions in %d tests\n", asserts_run, tests_run);

    return result != 0;
}