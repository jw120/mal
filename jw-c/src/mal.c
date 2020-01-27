/**
 *
 * mal.c - main program of the mal intepreter
 *
 **/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "history.h"
#include "printer.h"
#include "reader.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

mal READ(const char * s)
{
    return read_str(s);
}

mal EVAL(mal m)
{
    return m;
}

const char *PRINT(mal m)
{
    return pr_str(m, true);
}

mal read_eval(const char *s)
{
    mal m = READ(s);
    if (is_missing(m)) {
        return m;
    }
    return EVAL(m);
}

bool debug_mode;
enum { ECHO, READ_PRINT, FULL } mode = FULL;

int main()
{

    debug_mode = getenv("DEBUG") != NULL;
    const char *step = getenv("STEP");
    if (step && strcmp(step, "step0_repl") == 0) {
        mode = ECHO; // If running step0 we just echo the input
    }
    if (step && strcmp(step, "step1_read_print") == 0) {
        mode = READ_PRINT; // If running step0 we just read and print the input
    }

    start_history();
    while (true) {

        const char *input = readline("user> ");
        if (input == NULL) { // EOF from ctrl-D
            puts("");
            break;
        }
        add_history(input);

        if (mode == ECHO) {
            puts(input);
        } else {
            mal m = read_eval(input);
            if (!is_missing(m)) {
                puts(PRINT(m));
            }
        }

        fflush(stdout);
        free((void *) input);
    }
    end_history();
    return 0;
}
