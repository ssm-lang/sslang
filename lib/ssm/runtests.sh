#!/usr/bin/env bash

#########################
# SSM runtime test script
#########################
#
# Use this script to run all tests and compare against expected output, which
# can found in test/{examples,tests}.out). Example and test outputs are written
# to build/{examples,tests}.out, respectively.
#
# If Valgrind is installed, this script also uses it to check for memory errors.
# Valgrind output is written to build/{examples,tests}.vg-out.

set -euf
set -o pipefail

BUILD_DIR=./build
declare -a VG_FLAGS=("--leak-check=full" "--show-leak-kinds=all")

scriptname="$(basename "$0")"

say () {
  echo "[$scriptname]" "$@" >&2
}

run () {
  local exe="$BUILD_DIR/$1"
  shift
  echo "$exe" "$@"
  echo "$exe" "$@" >&2
  set +e
  "$exe" "$@" 2>&1 | sed 's/^/# /'
  local exit_code="$?"
  set -e
  if [ "$exit_code" -ne 0 ]; then
    say "Non-zero exit code $exit_code encountered while running:" "$exe" "$@"
    exit "$exit_code"
  fi
}

vg () {
  local exe="$BUILD_DIR/$1"
  shift
  echo valgrind "${VG_FLAGS[@]}" "$exe" "$@"
  echo valgrind "${VG_FLAGS[@]}" "$exe" "$@" >&2
  set +e
  valgrind "${VG_FLAGS[@]}" "$exe" "$@" 2>&1 | sed 's/^/# /'
  local exit_code="$?"
  set -e
  if [ "$exit_code" -ne 0 ]; then
    say "Non-zero exit code $exit_code encountered with valgrind:" "$exe" "$@"
    exit "$exit_code"
  fi
}

make clean
make exes tests

if ! [ -d "$BUILD_DIR" ] ; then
  echo "Could not find build directory: $BUILD_DIR. Quitting."
  exit 1
fi

rm -f "$BUILD_DIR/examples.out"
{
  run fib
  run fib 4
  run fib 5
  run fib 13
  run fib 15
  run onetwo
  run clock
  run counter
  run list
  run list 2048
  run closures
  run map-closure
} >> "$BUILD_DIR/examples.out"

if diff "$BUILD_DIR/examples.out" test/examples.out &> "$BUILD_DIR/examples.diff" ; then
  say "Example output matches expected."
else
  say "Example output differs from expected:"
  cat "$BUILD_DIR/examples.diff"
  exit 1
fi

rm -f "$BUILD_DIR/tests.out"
{
  run test_test-scheduler
} >> "$BUILD_DIR/tests.out"

if diff "$BUILD_DIR/tests.out" test/tests.out &> "$BUILD_DIR/tests.diff" ; then
  say "Test output matches expected."
else
  say "Test output differs from expected:"
  cat "$BUILD_DIR/tests.diff"
  exit 1
fi

make clean
make exes tests

if command -v valgrind >/dev/null ; then
  rm -f "$BUILD_DIR/examples.vg-out"
  {
    vg fib
    vg fib 4
    vg fib 5
    vg fib 13
    vg fib 15
    vg onetwo
    vg clock
    vg counter
    vg list
    vg list 2048
    vg closures
    vg map-closure
  } >> "$BUILD_DIR/examples.vg-out"
  say "Examples do not have any memory errors"

  {
    vg test_test-scheduler
  } >> "$BUILD_DIR/tests.out"
  say "Tests do not have any memory errors"
else
  say "Warning: valgrind not found in PATH. Skipping memory tests."
fi
