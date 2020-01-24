/**
 *
 * mal.c - main program of the mal intepreter
 *
 **/

#include <editline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define INPUT_BUFFER_SIZE 200

const char *pr_str(const char * s) {
    return s;
}

const char *read_str(const char * s) {
    return s;
}

const char *READ(const char * s) {
    return read_str(s);
}

const char *EVAL(const char *s) {
    return s;
}

const char *PRINT(const char *s) {
    return pr_str(s);
}

int main()
{
    while (true) {

        const char *input = readline("user> ");
        if (input == NULL) {
            puts("");
            break;
        }
        add_history(input);
        puts(PRINT(EVAL(READ(input))));
        free((void *) input);
    }
    return 0;
}
