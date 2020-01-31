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
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

mal READ(const char *s) { return read_str(s); }

mal EVAL(mal m, env *e) { return eval(m, e); }

const char *PRINT(mal m) { return pr_str(m, true); }

int main()
{

  // Turn on debug mode if the environment variable is set
  set_debug_level(getenv("DEBUG"));

  // To pass step0 and step 1 tests we need to restrict functionality
  enum
  {
    ECHO,
    READ_PRINT,
    FULL
  } mode = FULL;
  const char *step = getenv("STEP");
  if (step && strcmp(step, "step0_repl") == 0)
    mode = ECHO;
  if (step && strcmp(step, "step1_read_print") == 0)
    mode = READ_PRINT;

  // Set up our environment
  env *repl_env;
  if (mode == FULL)
  {
    DEBUG_HIGH_ENV(core_env());
    repl_env = env_new(NULL, core_env());
  }

  pre_history();
  while (true)
  {
    mal m;
    if (mode == FULL)
      DEBUG_HIGH_ENV(repl_env);
    const char *input = readline("user> ");
    if (input == NULL)
    { // EOF from ctrl-D
      puts("");
      break;
    }
    add_history(input);

    switch (mode)
    {
    case ECHO:
      puts(input);
      break;
    case FULL:
    case READ_PRINT:
      DEBUG_HIGH_FMT("input %s", input);
      m = READ(input);
      if (!is_missing(m))
      {
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
  return 0;
}
