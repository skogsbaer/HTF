#!/bin/bash

FLAGS="-hide-all-packages -package HTF -package base --make"
lineno=7

function check()
{
    grep_ecode="$1"
    if [ "$grep_ecode" != "0" ]
    then
        echo "Compile error did not occur in line $lineno, exit code of grep: ${grep_ecode}"
        exit 1
    fi
}

ghc $FLAGS Test1.hs 2>&1 | grep Test1.hs:$lineno
grep_ecode=${PIPESTATUS[1]}
check $grep_ecode

ghc $FLAGS Test2.hs 2>&1 | grep Test2.hs:$lineno
grep_ecode=${PIPESTATUS[1]}
check $grep_ecode
