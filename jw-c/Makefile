.PHONY: all

all: bin/step0_repl

CC = clang
CFLAGS = -Wall -O2


bin/% : src/%.c ; $(CC) $(CFLAGS) $< -o $@
