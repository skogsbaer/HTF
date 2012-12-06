#!/bin/bash

set -e

cabal configure --enable-tests
cabal build
cabal test
cabal install

cd tests
mkdir -p dist/build/htfpp
cp ../dist/build/htfpp/htfpp dist/build/htfpp
cabal configure --enable-tests
cabal build
cabal test

cd ../sample
mkdir -p dist/build/htfpp
cp ../dist/build/htfpp/htfpp dist/build/htfpp
cabal configure --enable-tests
cabal build
cabal test
