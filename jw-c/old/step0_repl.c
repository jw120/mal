// Step 0 - repl

#include <editline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_BUFFER_SIZE 200

const char *READ(const char * s)
{
    return s;
}

const char *EVAL(const char * s)
{
    return s;
}

const char *PRINT(const char* s)
{
    return s;
}

const char *rep(const char *s)
{
    return PRINT(EVAL(READ(s)));
}

int main()
{
    while (1) {

        const char *input = readline("user> ");
        if (input == NULL) {
            puts("");
            break;
        }
        add_history(input);
        puts(rep(input));
        free((void *) input);
    }
    return 0;
}
