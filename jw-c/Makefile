.PHONY: all build

all: build

build: bin/step0_repl

CC = clang
CFLAGS = -Wall -O2 -std=c17

bin/% : src/%.c ; $(CC) $(CFLAGS) $< -o $@
