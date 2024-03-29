#!/usr/bin/env bash

SSLC="stack exec sslc --"
pretty="--dump-ir-final"

# Run as, e.g.,
#    SSMDIR=../../ssm-runtime runtests.sh
# to use the runtime library in non-standard directory

if [ -z ${SSMDIR+x} ]
then
    SSMDIR="../lib/ssm"
fi
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
    echo "-t    Enable memory tracing"
    echo "-m    Use malloc only for the heap"
    echo "-v    Run valgrind on the generated executable (implies -m)"
    echo "-q    Suppress printing detailed log"
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
      SignalError "$2 not found"	
      echo "$2 not found" 1>&2
    else
	echo diff -b $1 $2 ">" $3 1>&2
	diff -b "$1" "$2" > "$3" 2>&1 || {
	    SignalError "$1 differs"
	    echo "FAILED $1 differs from $2" 1>&2
	    cat $3 >&2
	}
    fi
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
    Run $SSLC ${SSLCARGS} "$1" ">" "${csource}" && \
    Run $CC -c -o "${obj}" $EXTRA_CFLAGS "${csource}" && \
    Run $LINK -o "${exec}" "${obj}" $EXTRA_CFLAGS $platformdir/*.c -lssm -lpthread && \
    Run $valgrind "${exec}" ">" "${result}" && \
    Compare "${result}" "${reference}" "${diff}"

 #    # Pretty Printer Tests
 #    ir1="out/${basename}-ir1.ssl"
 #    ir2="out/${basename}-ir2.ssl"
 #    csourceIr2="out/${basename}-ir2.c"
 #    objIr2="out/${basename}-ir2.o"
 #    execIr2="out/${basename}-ir2"
 #    resultIr2="out/${basename}-ir2.out"
 #    diffIr2="out/${basename}-ir2.diff"
 #    diffIr3="out/${basename}-ir3.diff"
 #    NoteGen "${csourceIr2} ${ir1} ${ir2} ${objIr2} ${resultIr2}"
 #
 #    # TEST: can we pretty print the IR?
 #    Run $SSLC "$pretty" "$1" ">" "${ir1}" &&
 #    # TEST: can pretty printed IR be read back in?
 #    Run $SSLC "$pretty" "${ir1}" ">" "${ir2}" &&
 #    # TEST: can we fully compile pretty printed IR?
 #    Run $SSLC "${ir2}" ">" "out/${basename}-ir2.c" &&
 #    Run $CC -c -o "${objIr2}" "${csourceIr2}" &&
 #    if [ -f "${mainsource}" ] ; then
	# NoteGen "${ir1} ${ir2} ${objIr2} ${execIr2} ${result-ir2} ${diff-ir3} ${diff-ir2}"
	# Run $CC -c -o "${mainobj}" "${mainsource}" &&
	# Run $LINK -o "out/${basename}-ir2" "out/${basename}-ir2.o" "${mainobj}" -lssm &&
	# Run "${execIr2}" ">" "out/${basename}-ir2.out" &&
 #    # TEST: does compiled IR produce the same results as compiled source?
	# Compare "${resultIr2}" "${reference}" "out/${basename}-ir3.diff"
 #    fi
 #
 #    # Report the status and clean up the generated files
 #    

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

EXTRA_CFLAGS=-DCONFIG_MEM_STATS
valgrind=""
quiet=0

while getopts kctmvqh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	c) # Clean up any generated files
	    rm -f *.scanner-out *.scanner-diff $globallog
	    exit 0
	    ;;
	t) # Enable memory tracing
	    EXTRA_CFLAGS="$EXTRA_CFLAGS -DCONFIG_MEM_TRACE"
	    ;;
	m) # Enable using malloc-only
	    EXTRA_CFLAGS="$EXTRA_CFLAGS -DCONFIG_MALLOC_HEAP"
	    ;;
	v) # Enable running valgrind
	    # Turn on malloc-only
	    EXTRA_CFLAGS="$EXTRA_CFLAGS -DCONFIG_MALLOC_HEAP"
	    valgrind="valgrind --leak-check=full"
	    ;;
	q) # Turn off dumping runtests.log
	    quiet=1
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

# Rebuild the runtime library because the flags may have changed
Run make -C "$SSMDIR" clean "1>&2" 2>> $globallog
Run make -C "$SSMDIR" EXTRA_CFLAGS=\"$EXTRA_CFLAGS\" build/libssm.a "1>&2" 2>> $globallog

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

if [ "$quiet" -ne 1 ] && [ -n "$globalerror" ] && [ "$globalerror" -ne 0 ]; then
  cat "$globallog"
fi

exit $globalerror

