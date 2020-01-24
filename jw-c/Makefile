.PHONY: all build clean distclean run test

all: build test

# Directories - executables go in target, *.o and *.d in build
TARGET_DIR := .
BUILD_DIR := ./build
SRC_DIR := ./src

# Files
SRC_FILES := $(filter-out $(SRC_DIR)/step%,$(wildcard $(SRC_DIR)/*.c))
OBJ_FILES := $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(notdir $(basename $(SRC_FILES)))))
DEP_FILES := $(SRC_FILES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.d)
MAIN_OBJ_FILES = $(filter-out %/tests.o %_test.o,$(OBJ_FILES))
TEST_OBJ_FILES = $(filter-out %/mal.o,$(OBJ_FILES))

# Command options
CC = clang
CFLAGS = -Wall -O2 -std=c17
DEPFLAGS = -MMD -MT $@
LIBS= -lreadline -lpcre2-8

# Build rules

.PRECIOUS: $(DEP_FILES)

$(BUILD_DIR)/%.o : $(SRC_DIR)/%.c $(BUILD_DIR)/%.d
	$(CC) -c $(CFLAGS) $(DEPFLAGS) $(OUTPUT_OPTION) $<

$(TARGET_DIR)/mal: $(MAIN_OBJ_FILES)
	$(CC) $(LDFLAGS) $(LIBS) $^ -o $@

$(TARGET_DIR)/tests: $(TEST_OBJ_FILES)
	$(CC) $(LDFLAGS) $(LIBS) $^ -o $@

$(TARGET_DIR)/step0_repl: $(SRC_DIR)/step0_repl.c
	$(CC) $(CFLAGS) $(LIBS) $^ -o $@

# Dependencies

$(DEP_FILES):# Mentioned as dependencies so make won't fail when they don't exist
include $(wildcard $(DEP_FILES))

# Top-level targets

build: $(TARGET_DIR)/mal $(TARGET_DIR)/tests $(TARGET_DIR)/step0_repl

run: $(TARGET_DIR)/mal
	-@$(TARGET_DIR)/mal

test: $(TARGET_DIR)/tests
	-@$(TARGET_DIR)/tests

clean:
	$(RM) $(BUILD_DIR)/*

distclean: clean
	$(RM) $(TARGET_DIR)/mal $(TARGET_DIR)/tests $(TARGET_DIR)/step0_repl

