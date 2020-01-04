// Step 0 - repl

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
    while (true) {

        char input_buffer[INPUT_BUFFER_SIZE];

        printf("user> ");
        fflush(stdout);

        if (fgets(input_buffer, INPUT_BUFFER_SIZE, stdin) == NULL) {
            exit(!feof(stdin));
        }

        // remove trailing newline
        if (strlen(input_buffer) > 0) {
            input_buffer[strlen(input_buffer) - 1] = '\0';
        }

        puts(rep(input_buffer));
    }
    return 0;
}
