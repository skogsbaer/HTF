#!/bin/bash

cabal clean || exit 1
cabal configure || exit 1
cabal build || exit 1
dist/build/test/test --check || exit 1
echo "Tests ok"

cd compile-errors || exit 1
./run-tests.sh || exit 1
echo "Line numbers in compile errors ok"
