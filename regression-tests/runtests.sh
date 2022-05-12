#!/usr/bin/env bash

SSLC="stack exec sslc --"

SSMDIR="../lib/ssm"
SSMLIBDIR="${SSMDIR}/build"
SSMINC="${SSMDIR}/include"

CC="cc -O -g -Wall -Wno-unused-label -Wno-parentheses -pedantic -std=c99 -I ${SSMINC} -I out"
LINK="cc -g -I $SSMINC -L $SSMLIBDIR"

globallog=runtests.log
rm -f $globallog
error=
keep=

# Destination directory for generated files (log excepted)
mkdir -p out

# Set time limit
ulimit -t 5

Usage() {
    echo "Usage: runtests.sh [options] [files]"
    echo "-c    Clean up any existing generated files"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

# NoteGen <filename>
# Remember that the given filename was generated so we can clean up
NoteGen() {
  generatedfiles="$generatedfiles $1"
}

SignalError() {
    if [ -z "$error" ] ; then
        echo "FAILED"
        error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    NoteGen "$3"
    if ! [ -f "$2" ] ; then
      echo "$2 does not exist, nothing to compare" 1>&2
      return
    fi
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
      SignalError "$1 differs"
      echo "FAILED $1 differs from $2" 1>&2
      cat $3 >&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "failed: $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

# Run the compiler on the given file
Check() {
    error=
    basename=`basename $1 | sed 's/[.][^.]*$//'`
    reffile=`echo $1 | sed 's/[.][^.]*$//'`
    basedir=`dirname $1`

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=

    # Run the compiler

    csource="out/${basename}.c"
    cheader="out/${basename}.h"
    obj="out/${basename}.o"
    platformdir=${SSMDIR}/platform/posix/src
    exec="out/${basename}"
    result="out/${basename}.out"
    reference="${reffile}.out"
    diff="out/${basename}.diff"
    NoteGen "${csource} ${cheader} ${obj}"

    NoteGen "${exec} ${result} ${diff}"
    Run $SSLC "$1" ">" "${csource}" && \
    Run $CC -c -o "${obj}" "${csource}" && \
    Run $LINK -o "${exec}" "${obj}" $platformdir/*.c -lssm -lpthread && \
    Run "${exec}" ">" "${result}" && \
    Compare "${result}" "${reference}" "${diff}"

    # Report the status and clean up the generated files

    if [ -z "$error" ] ; then
	if [ -z "$keep" ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

# Run the compiler and expect an error
CheckFail() {
    error=
    basename=`basename $1 | sed 's/[.][^.]*$//'`
    reffile=`echo $1 | sed 's/[.][^.]*$//'`
    basedir=`dirname $1`

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing failure $basename" 1>&2

    generatedfiles=

    # Run the compiler

    csource="out/${basename}.c"
    errfile="out/${basename}.err"
    difffile="out/${basename}.diff"
    NoteGen "${csource} ${errfile} ${difffile}"
    RunFail $SSLC "$1" ">" "${csource}" "2>" "${errfile}" &&
    Compare "${errfile}" "${reffile}.err" "${difffile}"

    # Report the status and clean up the generated files

    if [ -z "$error" ] ; then
	if [ -z "$keep" ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}


while getopts kch c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	c) # Clean up any generated files
	    rm -f *.scanner-out *.scanner-diff $globallog
	    exit 0
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/*.ssl"
fi

Run make -C "$SSMDIR" build/libssm.a "1>&2" 2>> $globallog

for file in $files
do
  case "$file" in
    *-fail.ssl)
	  CheckFail "$file" 2>> $globallog
	  ;;
    *)
	  Check "$file" 2>> $globallog
	  ;;
  esac    
done

if [ -n "$globalerror" ] && [ "$globalerror" -ne 0 ]; then
  cat "$globallog"
fi

exit $globalerror

