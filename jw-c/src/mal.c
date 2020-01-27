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

int main()
{

    debug_mode = getenv("DEBUG") != NULL;

    start_history();
    while (true) {

        const char *input = readline("user> ");
        if (input == NULL) { // EOF from ctrl-D
            puts("");
            break;
        }
        add_history(input);
        mal m = read_eval(input);
        if (!is_missing(m)) {
            puts(PRINT(m));
        }
        fflush(stdout);
        free((void *) input);
    }
    end_history();
    return 0;
}
