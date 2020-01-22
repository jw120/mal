.PHONY: all build clean test

all: build

build: mal

CC = clang
CFLAGS = -Wall -O2 -std=c17
LIBS= -lreadline -lpcre2-8

mal: list.o mal.o mal_types.o printer.o reader.o tokenize.o utils.o

list.o: src/mal_types.h src/list.h
mal_types.o: src/mal_types.h
mal.o: src/mal_types.h src/reader.h src/tokenize.h src/utils.h
printer.o: src/mal_types.h src/printer.h src/utils.h
reader.o: src/mal_types.h src/reader.h src/list.h src/tokenize.h src/utils.h
tokenize.o: src/mal_types.h src/tokenize.h src/utils.h
utils.o: src/mal_types.h src/utils.h

mal: list.o mal.o mal_types.o printer.o reader.o tokenize.o utils.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

test_all: test_all.o test_list.o list.o mal_types.o utils.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

%.o: src/%.c
	$(CC) $(CFLAGS) -c $<

test: test_all
	./test_all

clean:
	-rm bin/* *.o
