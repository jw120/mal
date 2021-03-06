/**
 *
 * mal.c - main program of the mal intepreter
 *
 **/

#include <stdio.h> // stdio has to be before readline/history.h

#include <assert.h>
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

env *repl_env;

// Fundamental function to convert a string into a mal ast
// Defined by the https://github.com/kanaka/mal instructions
static mal READ(const char *s) { return read_str(s); }

// Fundamental function to value a mal ast to a mal value
// Defined by the https://github.com/kanaka/mal instructions
static mal EVAL(mal m, env *e) { return eval(m, e); }

// Fundamental function to convert a mal value into a string
// Defined by the https://github.com/kanaka/mal instructions
static const char *PRINT(mal m) { return pr_str(m, true); }

// C implementation of mal eval to be added to the environment
static mal mal_eval(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to eval");
  return EVAL(n->val, repl_env);
}

// Name of the arguments symbol in mal
#define ARGV_NAME "*ARGV*"

// Called from main to set the arguments that the C program was called with
// in the mal environment
static void setup_mal_argv(int count, char *args[], env *e) {

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

#define LOAD_FILE_PREFIX "(load-file \""
#define LOAD_FILE_SUFFIX "\")"

// Called from main to load a file on startup
static mal load_file(char *filename, env *e) {

  DEBUG_INTERNAL_FMT("loading file %s", filename);
  size_t buf_size = strlen(LOAD_FILE_PREFIX) + strlen(filename) +
                    strlen(LOAD_FILE_SUFFIX) + 1;
  char *buf = checked_malloc(buf_size, "load_file filename");
  strncpy(buf, LOAD_FILE_PREFIX, buf_size);
  strncat(buf, filename, buf_size - strlen(buf));
  strncat(buf, LOAD_FILE_SUFFIX, buf_size - strlen(buf));
  DEBUG_INTERNAL_FMT("read-eval  %s", buf);

  mal ret = EVAL(READ(buf), e);
  if (is_exception(ret)) {
    PRINT(ret);
    exit(EXIT_FAILURE);
  }
  free(buf);
  return ret;
}

int main(int argc, char *argv[]) {

  set_debug_level_from_env();

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

  if (mode == FULL)
    assert(eval(read_str("(println (str \"Mal [\" *host-language* \"]\"))"),
                repl_env)
               .tag == NIL);

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
