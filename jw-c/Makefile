.PHONY: all build clean distclean format run test step-test run-debug

all: build test step-test

# Directories - executables go in target, *.o and *.d in build
TARGET_DIR := ./exec
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
CFLAGS = -Wall -O2 -std=c17 -I/usr/local/opt/readline/include
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

# Dependencies

$(DEP_FILES):# Mentioned as dependencies so make won't fail when they don't exist
include $(wildcard $(DEP_FILES))

# Top-level targets

build: $(TARGET_DIR)/mal $(TARGET_DIR)/tests

run: $(TARGET_DIR)/mal
	-@$(TARGET_DIR)/mal

run-debug:  $(TARGET_DIR)/mal
	-@DEBUG=1 $(TARGET_DIR)/mal

test: $(TARGET_DIR)/tests
	-@$(TARGET_DIR)/tests

step-test: $(TARGET_DIR)/mal
	-@echo step 0; cd ..; make "test^jw-c^step0" | grep "failing tests"
	-@echo step 1; cd ..; make "test^jw-c^step1" | grep "failing tests"
	-@echo step 2; cd ..; make "test^jw-c^step2" | grep "failing tests"
	-@echo step 3; cd ..; make "test^jw-c^step3" | grep "failing tests"
	-@echo step 4; cd ..; make "test^jw-c^step4" | grep "failing tests"
	-@echo step 5; cd ..; make "test^jw-c^step5" | grep "failing tests"
	-@echo step 6; cd ..; make "test^jw-c^step6" | grep "failing tests"
	-@echo step 7; cd ..; make "test^jw-c^step7" | grep "failing tests"
	-@echo step 8; cd ..; make "test^jw-c^step8" | grep "failing tests"


clean:
	$(RM) $(BUILD_DIR)/*

distclean: clean
	$(RM) $(TARGET_DIR)/*

format:
	clang-format -i src/*.c src/*.h

