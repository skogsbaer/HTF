#!/bin/bash

if [ "$1" != "--no-clean" ]
then
    cabal clean || exit 1
fi
mkdir -p dist/build/htfpp || exit 1
pushd ..
cabal build || exit 1
popd
cp ../dist/build/htfpp/htfpp dist/build/htfpp || exit 1
cabal-1.16.0 configure || exit 1
cabal-1.16.0 build || exit 1
dist/build/test/test || exit 1
echo "Tests ok"

./compile-errors/run-tests.sh || exit 1
echo "Line numbers in compile errors ok"
