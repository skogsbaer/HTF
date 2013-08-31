#!/bin/bash

set -e
ghc-pkg unregister HTF || true

cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test || exit 1
cabal install || exit 1

cd sample
cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test || exit 1
