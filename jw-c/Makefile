.PHONY: all build clean

all: build

build: bin/step0_repl bin/step1_read_print


CC = clang
CFLAGS = -Wall -O2 -std=c17
LIBS= -lreadline -lpcre

bin/step1_read_print: reader.o utils.o
reader.o: utils.o

bin/%: %.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

%.o: src/%.c
	$(CC) $(CFLAGS) -c $<

clean:
	-rm bin/* *.o
