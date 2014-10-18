#!/bin/bash

cd "$(dirname $0)"

FLAGS="-hide-all-packages -package base -package-db ../../dist/package.conf.inplace -package HTF --make"
SANDBOX_FILE="../../cabal.sandbox.config"
SANDBOX_DIR="../../.cabal-sandbox"
GHC_VERSION=$(ghc --version | sed 's/.*version //g')

if [ -e "$SANDBOX_FILE"  ]
then
    for pkg_db in $(ls -d -1 $SANDBOX_DIR/*$GHC_VERSION*-packages.conf.d)
    do
        FLAGS="$FLAGS -package-db $pkg_db"
    done
fi
echo $FLAGS

lineno=7
function check()
{
    test="$1"
    ghc $FLAGS "$test" 2>&1 | grep "$test":$lineno
    grep_ecode=${PIPESTATUS[1]}
    if [ "$grep_ecode" != "0" ]
    then
        echo "Compile error for $test did not occur in line $lineno, exit code of grep: ${grep_ecode}"
        ghc $FLAGS "$test"
        exit 1
    fi
}

check Test1.hs
check Test2.hs
check Test3.hs
check Test4.hs
