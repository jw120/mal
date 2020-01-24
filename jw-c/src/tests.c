/**
 *
 * tests.c - main program for unit tests
 *
 **/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "minunit.h"

#include "list_test.h"
#include "printer_test.h"
#include "reader_test.h"
#include "tokenize_test.h"
#include "types_test.h"

int asserts_run = 0;
int tests_run = 0;

const char *run_tests() {
    mu_run_test(list_test);
    mu_run_test(printer_test);
    mu_run_test(reader_test);
    mu_run_test(tokenize_test);
    mu_run_test(types_test);
    return 0;
 }

bool debug_mode;

int main(int argc, char **argv) {

    debug_mode = getenv("DEBUG") != NULL;

    const char *result = run_tests();

    if (result != 0) {
        printf("%s\n", result);
    } else {
        printf("ALL TESTS PASSED - ");
    }
    printf("Tested %d assertions in %d tests\n", asserts_run, tests_run);

    return result != 0;
}