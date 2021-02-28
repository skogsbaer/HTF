#!/bin/bash

cd "$(dirname $0)"

lineno=7
function check()
{
    test="$1"
    echo "Running test $test ..."
    command -v grep >/dev/null 2>&1 || \
        { echo >&2 "Test $0 requires ``grep'' but it's not installed.  Aborting."; exit 1; }
    compile_cmd="stack $HTF_TEST_STACK_ARGS ghc --package HTF -- -XPackageImports $test"
    $compile_cmd 2>&1 | grep "$test":$lineno > /dev/null
    grep_ecode=${PIPESTATUS[1]}
    if [ "$grep_ecode" != "0" ]
    then
        echo "Compile error for $test did not occur in line $lineno, exit code of grep: ${grep_ecode}"
        echo "Compile command: $compile_cmd"
        echo "Stack version: $(stack --version)"
        echo "GHC version: $(stack $HTF_TEST_STACK_ARGS ghc -- --version)"
        echo "Here is the output of the compiler:"
        $compile_cmd
        exit 1
    fi
    echo "Done with test $test"
}

check Test1.hs
check Test2.hs
check Test3.hs
check Test4.hs
