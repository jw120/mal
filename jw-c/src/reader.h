#ifndef READER_H
#define READER_H

typedef struct reader_state reader_state;
struct reader_state {
    const char *input_string;
    int input_length;
    const char *current;
    int offset;
};

void read_str(const char *);

#endif