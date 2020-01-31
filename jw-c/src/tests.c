/**
 *
 * tests.c - main program for unit tests
 *
 **/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "minunit.h"

#include "core_misc_test.h"
#include "core_num_test.h"
#include "core_seq_test.h"
#include "env_test.h"
#include "eval_test.h"
#include "map_test.h"
#include "printer_test.h"
#include "reader_test.h"
#include "seq_test.h"
#include "tokenize_test.h"
#include "types_test.h"
#include "utils_test.h"

int asserts_run = 0;
int tests_run = 0;

const char *run_tests()
{
  mu_run_test(core_misc_test);
  mu_run_test(core_num_test);
  mu_run_test(core_seq_test);
  mu_run_test(env_test);
  mu_run_test(eval_test);
  mu_run_test(map_test);
  mu_run_test(printer_test);
  mu_run_test(reader_test);
  mu_run_test(seq_test);
  mu_run_test(tokenize_test);
  mu_run_test(types_test);
  mu_run_test(utils_test);
  return 0;
}

bool debug_mode;

int main(int argc, char **argv)
{

  debug_mode = getenv("DEBUG") != NULL;

  const char *result = run_tests();

  if (result != 0)
    printf("%s\n", result);
  else
    printf("ALL TESTS PASSED - ");
  printf("Tested %d assertions in %d tests\n", asserts_run, tests_run);

  return result != 0;
}