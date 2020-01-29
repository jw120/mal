#ifndef TOKENIZE_H
#define TOKENIZE_H

typedef struct token_result token_result;

struct token_result {
  const char *val;
  int next_offset;
};

const token_result *tokenize(const char *input_string, const int);

#endif