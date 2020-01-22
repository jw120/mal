#include <string.h>
#include "minunit.h"

#include "tokenize_test.h"
#include "tokenize.h"

const char *tokenize_test() {

    const char *t = "1234    abc  \"123\"";
    //               012345678901234567890

    const token_result *t_num = tokenize(t, 0);
    mu_assert("tokenize number value", strcmp(t_num->val, "1234") == 0);
    mu_assert("tokenize number offset ", t_num->next_offset == 4);

    return 0;
}
