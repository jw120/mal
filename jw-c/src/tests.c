/**
 *
 * tests.c - main program for unit tests
 *
 **/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "minunit.h"

#include "debug.h"

#include "core_atom_test.h"
#include "core_is_test.h"
#include "core_mal_test.h"
#include "core_misc_test.h"
#include "core_num_test.h"
#include "core_seq_test.h"
#include "env_test.h"
#include "eval_test.h"
#include "hash_table_test.h"
#include "printer_test.h"
#include "reader_test.h"
#include "seq_test.h"
#include "tokenize_test.h"
#include "types_test.h"
#include "utils_test.h"

int asserts_run = 0;
int tests_run = 0;

const char *run_tests(void);

const char *run_tests(void) {
  mu_run_test_function(core_atom_test);
  mu_run_test_function(core_is_test);
  mu_run_test_function(core_mal_test);
  mu_run_test_function(core_misc_test);
  mu_run_test_function(core_num_test);
  mu_run_test_function(core_seq_test);
  mu_run_test_function(env_test);
  mu_run_test_function(eval_test);
  mu_run_test_function(hash_table_test);
  mu_run_test_function(printer_test);
  mu_run_test_function(reader_test);
  mu_run_test_function(seq_test);
  mu_run_test_function(tokenize_test);
  mu_run_test_function(types_test);
  mu_run_test_function(utils_test);
  return 0;
}

int main(UNUSED(int argc), UNUSED(char **_argv)) {

  set_debug_level_from_env();

  const char *result = run_tests();

  if (result != 0)
    printf("%s\n", result);
  else
    printf("ALL TESTS PASSED - ");
  printf("Tested %d assertions in %d tests\n", asserts_run, tests_run);

  return result != 0;
}
