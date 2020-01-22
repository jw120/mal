.PHONY: all clean run-test run

all: mal run-test

CC = clang
CFLAGS = -Wall -O2 -std=c17
LIBS= -lreadline -lpcre2-8

mal: list.o mal.o types.o printer.o reader.o tokenize.o utils.o

list.o: src/types.h src/list.h
types.o: src/types.h
mal.o: src/types.h src/reader.h src/tokenize.h src/utils.h
printer.o: src/types.h src/printer.h src/utils.h
reader.o: src/types.h src/reader.h src/list.h src/tokenize.h src/utils.h
tokenize.o: src/types.h src/tokenize.h src/utils.h
utils.o: src/types.h src/utils.h

mal: list.o mal.o types.o printer.o reader.o tokenize.o utils.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

test: test.o types.o utils.o \
	list_test.o list.o \
	printer_test.o printer.o \
	reader_test.o reader.o \
	tokenize_test.o tokenize.o list.o
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

run: mal
	./mal

run-test: test
	./test

%.o: src/%.c
	$(CC) $(CFLAGS) -c $<

clean:
	-rm *.o mal test_all

