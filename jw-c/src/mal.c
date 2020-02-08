/**
 *
 * mal.c - main program of the mal intepreter
 *
 **/

#include <stdio.h> // stdio has to be before readline/history.h

#include <readline/history.h>
#include <readline/readline.h>
#include <stdbool.h>
#include <stdlib.h>

#include "core.h"
#include "debug.h"
#include "eval.h"
#include "history.h"
#include "printer.h"
#include "reader.h"
#include "seq.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

env *repl_env;

mal READ(const char *s) { return read_str(s); }

mal EVAL(mal m, env *e) { return eval(m, e); }

const char *PRINT(mal m) { return pr_str(m, true); }

// C implementation of mal eval
mal mal_eval(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to eval");
  return EVAL(n->val, repl_env);
}

#define ARGV_NAME "*ARGV*"

void setup_mal_argv(int count, char *args[], env *e) {

  DEBUG_INTERNAL_FMT("adding argv with %d args", count);
  list_node *args_list_head = NULL;
  list_node *n = NULL;

  for (int i = 0; i < count; i++) {
    n = list_extend(mal_str(args[i]), n);
    if (args_list_head == NULL)
      args_list_head = n;
  }
  mal mal_args = mal_list(args_list_head); // (arg1 arg2 arg3)
  DEBUG_INTERNAL_MAL("eval args", mal_args);
  mal quoted_args =
      mal_cons(mal_sym("quote"), mal_cons(mal_args, mal_list(NULL)));
  DEBUG_INTERNAL_MAL("eval quoted_args", quoted_args);
  mal command = mal_cons(
      mal_sym("def!"),
      mal_cons(mal_sym(ARGV_NAME), mal_cons(quoted_args, mal_list(NULL))));
  DEBUG_INTERNAL_MAL("eval", command);
  mal ret = EVAL(command, e);
  DEBUG_INTERNAL_MAL("eval ret", ret);
  if (is_exception(ret)) {
    PRINT(ret);
    exit(EXIT_FAILURE);
  }
}

// (a b c) = a : b : c : []
// (a (b c)) = a : (b : c : [])

#define LOAD_FILE_PREFIX "(load-file \""
#define LOAD_FILE_SUFFIX "\")"

mal load_file(char *filename, env *e) {

  DEBUG_INTERNAL_FMT("loading file %s", filename);
  size_t buf_size = strlen(LOAD_FILE_PREFIX) + strlen(filename) +
                    strlen(LOAD_FILE_SUFFIX) + 1;
  char *buf = checked_malloc(buf_size, "load_file filename");
  strncpy(buf, LOAD_FILE_PREFIX, buf_size);
  strncat(buf, filename, buf_size);
  strncat(buf, LOAD_FILE_SUFFIX, buf_size);
  DEBUG_INTERNAL_FMT("read-eval  %s", buf);

  mal ret = EVAL(READ(buf), e);
  if (is_exception(ret)) {
    PRINT(ret);
    exit(EXIT_FAILURE);
  }
  return ret;
}

int main(int argc, char *argv[]) {

  // Turn on debug mode if the environment variable is set
  set_debug_level(getenv("DEBUG"));

  // To pass step 0 and step 1 tests we need to restrict functionality
  enum { ECHO, READ_PRINT, FULL } mode = FULL;
  const char *step = getenv("STEP");
  if (step && strcmp(step, "step0_repl") == 0)
    mode = ECHO;
  if (step && strcmp(step, "step1_read_print") == 0)
    mode = READ_PRINT;

  // Set up our environment
  if (mode == FULL) {
    DEBUG_HIGH_ENV(core_env());
    env_set(core_env(), "eval", mal_fn(mal_eval));
    repl_env = env_new(NULL, core_env());
    env_set(repl_env, ARGV_NAME, mal_list(NULL));

    if (argc > 1) {
      DEBUG_INTERNAL_FMT("calling setup_mal_argv");
      setup_mal_argv(argc - 2, argv + 2, repl_env);
      DEBUG_INTERNAL_FMT("calling load-file");
      mal ret = load_file(argv[1], repl_env);
      if (is_exception(ret)) {
        printf("Failed in loading file %s", argv[0]);
        PRINT(ret);
        return EXIT_FAILURE;
      }
      return EXIT_SUCCESS;
    }
  }

  pre_history();
  while (true) {
    mal m;
    if (mode == FULL)
      DEBUG_HIGH_ENV(repl_env);
    const char *input = readline("user> ");
    if (input == NULL) { // EOF from ctrl-D
      puts("");
      break;
    }
    add_history(input);

    switch (mode) {
    case ECHO:
      puts(input);
      break;
    case FULL:
    case READ_PRINT:
      DEBUG_HIGH_FMT("input %s", input);
      m = READ(input);
      if (!is_missing(m)) {
        DEBUG_HIGH_MAL("read", m);
        if (mode == FULL)
          m = EVAL(m, repl_env);
        puts(PRINT(m));
      }
      break;
    }
    fflush(stdout);
    free((void *)input);
  }
  post_history();
  return EXIT_SUCCESS;
}
