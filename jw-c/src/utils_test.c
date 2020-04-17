#include <string.h>

#include "minunit.h"
#include "seq.h"
#include "utils.h"
#include "utils_test.h"

// #define MAX_CHARS 8

const char *utils_test() {

  list_node *n = list_cons(mal_str("a"), list_cons(mal_str("bc"), NULL));
  mu_assert("str_join",
            strcmp(str_join(n, 3, 2, "!", "<", ">"), "<a!bc>") == 0);
  mu_assert("str_join", strcmp(str_join(NULL, 0, 0, "!", "<", ">"), "<>") == 0);

  return 0;
}
