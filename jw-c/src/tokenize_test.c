#include <string.h>

#include "minunit.h"
#include "tokenize.h"
#include "tokenize_test.h"

const char *tokenize_test() {

  const char *t = "1234    abc  \"123\"";
  //               0123456789012 3456 7890

  const tokenize_result *t_num = tokenize(t, 0);
  mu_assert("tokenize number value", strcmp(t_num->val, "1234") == 0);
  mu_assert("tokenize number offset ", t_num->next_offset == 4);

  const tokenize_result *t_sym = tokenize(t, 4);
  mu_assert("tokenize sym value", strcmp(t_sym->val, "abc") == 0);
  mu_assert("tokenize sym offset ", t_sym->next_offset == 11);

  const tokenize_result *t_str = tokenize(t, 11);
  mu_assert("tokenize str value", strcmp(t_str->val, "\"123\"") == 0);
  mu_assert("tokenize str offset ", t_str->next_offset == 18);

  const tokenize_result *t_null = tokenize(t, 18);
  mu_assert("tokenize null value", strcmp(t_null->val, "") == 0);

  return 0;
}
