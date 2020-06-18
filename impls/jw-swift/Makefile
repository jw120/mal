.PHONY: all build clean distclean format run test step-test self-host-test run-debug

#all: build test step-test self-host-test
all: step-test

# # Directories - executables go in target, *.o and *.d in build
TARGET_DIR := ./Build
# BUILD_DIR := ./build
# SRC_DIR := ./src

# # Files
# SRC_FILES := $(filter-out $(SRC_DIR)/step%,$(wildcard $(SRC_DIR)/*.c))
# OBJ_FILES := $(addprefix $(BUILD_DIR)/,$(addsuffix .o,$(notdir $(basename $(SRC_FILES)))))
# DEP_FILES := $(SRC_FILES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.d)
# MAIN_OBJ_FILES = $(filter-out %/tests.o %_test.o,$(OBJ_FILES))
# TEST_OBJ_FILES = $(filter-out %/mal.o,$(OBJ_FILES))

# Top-level targets

#build: mkdirs $(TARGET_DIR)/mal $(TARGET_DIR)/tests

run: $(TARGET_DIR)/mal
	-@$(TARGET_DIR)/mal

#run-debug:  $(TARGET_DIR)/mal
#	-@DEBUG=1 $(TARGET_DIR)/mal

#test: $(TARGET_DIR)/tests
#	-@$(TARGET_DIR)/tests

step-test: $(TARGET_DIR)/mal
	-@echo step 0; cd ../..; make "test^jw-c^step0" | grep "failing tests"
	-@echo step 1; cd ../..; make "test^jw-c^step1" | grep "failing tests"
#	-@echo step 2; cd ../..; make "test^jw-c^step2" | grep "failing tests"
#	-@echo step 3; cd ../..; make "test^jw-c^step3" | grep "failing tests"
#	-@echo step 4; cd ../..; make "test^jw-c^step4" | grep "failing tests"
#	-@echo step 5; cd ../..; make "test^jw-c^step5" | grep "failing tests"
#	-@echo step 6; cd ../..; make "test^jw-c^step6" | grep "failing tests"
#	-@echo step 7; cd ../..; make "test^jw-c^step7" | grep "failing tests"
#	-@echo step 8; cd ../..; make "test^jw-c^step8" | grep "failing tests"
#	-@echo step 9; cd ../..; make "test^jw-c^step9" | grep "failing tests"
#	-@echo step A; cd ../..; make "test^jw-c^stepA" | grep "failing tests"

#self-host-test:
#	@cd ../..; make MAL_IMPL=jw-c "test^mal" | grep "TEST RESULTS\|failing tests"

#clean:
#	$(RM) $(BUILD_DIR)/*

#distclean: clean#
#	$(RM) $(TARGET_DIR)/*

#format:
#	clang-format -i src/*.c src/*.h

