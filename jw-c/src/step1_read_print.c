// Step 0 - repl

#include <editline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "reader.h"
#include "tokenize.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

const char *READ(const char * s)
{
    read_str(s);
    debug("READ", "read_str finished (have '%s')", s || "NULL");
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
    read_str("  QQ  2");
    debug("main", "back");
    return 0;

    // while (1) {

    //     const char *input = readline("user> ");
    //     if (input == NULL) {
    //         puts("");
    //         break;
    //     }
    //     add_history(input);
    //     puts(rep(input));
    //     free((void *) input);
    // }
    // return 0;
}
