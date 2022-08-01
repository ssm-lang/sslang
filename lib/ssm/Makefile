######################
# SSM runtime Makefile
######################
#
# For a list of useful available targets, run `make help'.
#
# Note that this Makefile is only used to build and test the platform-generic
# core of the SSM runtime; platform-specific builds may use their own build
# system that ignores this Makefile.
#
# Any .c files under EXE_DIR are linked against this library to produce an
# executable.
#
# This Makefile also a version of the runtime library instrumented for code
# coverage analysis. The build artifacts for instrumented code are mangled using
# the test_ prefix. This version of the runtime library is used to link against
# .c files in TEST_DIR.
#
# To actually run tests, please see the runtests.sh script.

LIB_NAME := ssm

MAKE_CC ?= cc
MAKE_LD ?= $(MAKE_CC)
MAKE_AR ?= ar

PLATFORM ?= simulation

BUILD_DIR := build
SRC_DIR := src
INC_DIR := include
TEST_DIR := test
EXE_DIR := examples
DOC_DIR := doc
PLATFORM_DIR := platform/$(PLATFORM)
GCOVR_CFG := test/gcovr.cfg

LIB_SRC := $(wildcard $(SRC_DIR)/*.c)
LIB_INC := $(wildcard include/*.h)

LIB_OBJ := $(patsubst $(SRC_DIR)/%.c, $(BUILD_DIR)/%.o, $(LIB_SRC))
LIB_TGT := $(BUILD_DIR)/lib$(LIB_NAME).a

DOC_CFG := doc/Doxyfile
DOC_SRC := README.md $(wildcard doc/*.md doc/*.dox) $(LIB_SRC) $(LIB_INC)
DOC_TGT := $(BUILD_DIR)/doc

EXE_SRC := $(wildcard $(EXE_DIR)/*.c)
EXE_OBJ := $(patsubst $(EXE_DIR)/%.c, $(BUILD_DIR)/%.o, $(EXE_SRC))
EXE_TGT := $(patsubst %.o, %, $(EXE_OBJ))
EXE_INC := $(wildcard $(EXE_DIR)/*.h)

TLIB_NAME := t$(LIB_NAME)
TLIB_OBJ := $(patsubst $(SRC_DIR)/%.c, $(BUILD_DIR)/test_%.o, $(LIB_SRC))
TLIB_TGT := $(BUILD_DIR)/lib$(TLIB_NAME).a

TEST_SRC := $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ := $(patsubst $(TEST_DIR)/%.c, $(BUILD_DIR)/test_%.o, $(TEST_SRC))
TEST_TGT := $(patsubst %.o, %, $(TEST_OBJ))

PLATFORM_SRC := $(wildcard $(PLATFORM_DIR)/src/*.c)
PLATFORM_OBJ := $(patsubst $(PLATFORM_DIR)/src/%.c, $(BUILD_DIR)/%.o, $(PLATFORM_SRC))
$(info PLATFORM is $(PLATFORM))

COV_TGT := $(BUILD_DIR)/coverage.xml

CC = $(MAKE_CC)
CFLAGS += -g -I$(INC_DIR) -O -Wall -pedantic -DSSM_TIMER64_PRESENT

# # Check whether valgrind is available.
# ifeq ($(shell which valgrind),)
# $(info # Valgrind is not available; compiling without it.)
# else
# # If available, we try to #include <valgrind/valgrind.h>, which we use to
# # instrument our memory allocator.
# #
# # NOTE: this is just a heuristic that may not be very reliable.
# # We should probably implement better dependency management.
# $(info # Valgrind seems to be available; compiling libssm with Valgrind support.)
# CFLAGS += -DUSE_VALGRIND
# endif

TEST_CFLAGS = $(CFLAGS) -g -DSSM_DEBUG --coverage

LD = $(MAKE_LD)
LDFLAGS = -L$(BUILD_DIR)
TEST_LDFLAGS = $(LDFLAGS) --coverage
LDLIBS= -lpthread

AR = $(MAKE_AR)
ARFLAGS = -cr

PHONY += lib exes tests cov docs
lib: $(LIB_TGT)
exes: $(EXE_TGT)
tests: $(TEST_TGT)
cov: $(COV_TGT)
docs: $(DOC_TGT)

$(LIB_TGT): $(LIB_OBJ)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $+

$(TLIB_TGT): $(TLIB_OBJ)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $+

$(EXE_TGT): %: %.o $(PLATFORM_OBJ) $(LIB_TGT)
	$(LD) $(LDFLAGS) -o $@ $@.o $(PLATFORM_OBJ) -l$(LIB_NAME) $(LDLIBS)

$(TEST_TGT): %: %.o $(TLIB_TGT)
	$(LD) $(TEST_LDFLAGS) -o $@ $@.o -l$(TLIB_NAME) $(LDLIBS)

vpath %.c $(SRC_DIR) $(EXE_DIR) $(TEST_DIR) $(PLATFORM_DIR)/src

$(EXE_OBJ): $(BUILD_DIR)/%.o: %.c $(LIB_INC) $(EXE_INC) | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(LIB_OBJ): $(BUILD_DIR)/%.o: %.c $(LIB_INC) | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(TLIB_OBJ) $(TEST_OBJ): $(BUILD_DIR)/test_%.o: %.c $(LIB_INC) | $(BUILD_DIR)
	rm -f $(patsubst %.o, %.gcda, $@) $(patsubst %.o, %.gcno, $@)
	$(CC) $(TEST_CFLAGS) -c -o $@ $<

$(PLATFORM_OBJ): $(BUILD_DIR)/%.o: %.c $(LIB_INC) | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(COV_TGT): $(GCOVR_CFG) $(TEST_TGT)
	@for i in $(TEST_TGT) ; do \
		echo ./$$i ;\
		./$$i >/dev/null || exit $$? ;\
	done
	gcovr --config $(GCOVR_CFG)

$(DOC_TGT): $(DOC_CFG) $(DOC_SRC) | $(BUILD_DIR)
	doxygen doc/Doxyfile

$(BUILD_DIR):
	mkdir -p $@

PHONY += clean
clean:
	rm -rf build

PHONY += help
help:
	@echo "Available phony targets:" $(PHONY)
	@echo
	@echo "lib     Build SSM runtime library     [build/libssm.a]"
	@echo "exes    Build programs in examples/   [build/<example-name>]"
	@echo "tests   Build tests in test/          [build/test_<test-name>]"
	@echo "cov     Build coverage report         [build/<filename.c>.gcov] (depends on gcovr)"
	@echo "docs    Build code documentation      [build/doc]               (depends on doxygen)"
	@echo "clean   Remove build directory"
	@echo "help    Show this help menu"
	@echo
	@echo "Available example targets:" $(EXE_TGT)
	@echo "Available test targets:" $(TEST_TGT)

.PHONY: $(PHONY)
