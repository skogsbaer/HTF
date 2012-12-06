#!/bin/bash

set -e

cabal configure --enable-tests
cabal build
cabal test
cabal install

cd tests
cabal configure --enable-tests
cabal build
cabal test

cd ../sample
cabal configure --enable-tests
cabal build
cabal test
