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
#include "eval.h"
#include "history.h"
#include "printer.h"
#include "reader.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

mal READ(const char *s) { return read_str(s); }

mal EVAL(mal m, env *e) { return eval(m, e); }

const char *PRINT(mal m) { return pr_str(m, true); }

// Debugging controlled by this global variable
bool debug_mode;

int main()
{

  // Turn on debug mode if the environment variable is set
  debug_mode = getenv("DEBUG") != NULL;

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
    repl_env = core_env();

  start_history();
  while (true)
  {
    mal m;
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
      m = READ(input);
      if (!is_missing(m))
      {
        if (mode == FULL)
          m = EVAL(m, repl_env);
        puts(PRINT(m));
      }
      break;
    }
    fflush(stdout);
    free((void *)input);
  }
  end_history();
  return 0;
}
