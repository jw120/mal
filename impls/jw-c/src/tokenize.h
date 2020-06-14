#ifndef TOKENIZE_H
#define TOKENIZE_H

// Return values of the tokenize function
typedef struct tokenize_result {
  const char *val;
  size_t next_offset;
} tokenize_result;

// Returns the next mal token in the input string starting at the given offset
// Token value is returned along with the offset for the next character after
// the token, so the function can be called again to find the next token.
// If no token is found, then returns NULL
const tokenize_result *tokenize(const char *input_string, const size_t offset);

#endif
