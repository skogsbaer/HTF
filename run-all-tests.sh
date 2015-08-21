#!/bin/bash

SANDBOX_FILE="cabal.sandbox.config"
SANDBOX_DIR=".cabal-sandbox"
CABAL=$HOME/bin/cabal-with-cpphs

if [ ! -x "$CABAL" ]; then
    CABAL=cabal
fi

set -e
if [ -e "$SANDBOX_FILE"  ]
then
    have_sandbox=yes
    cabal sandbox hc-pkg unregister HTF || true
else
    have_sandbox=no
    ghc-pkg unregister HTF || true
fi

$CABAL configure --enable-tests || exit 1
$CABAL build || exit 1
$CABAL test || exit 1
$CABAL install || exit 1

cd sample || exit 1

if [ "$have_sandbox" == "yes" ]
then
    rm -f cabal.sandbox.config
    cabal sandbox init --sandbox ../"$SANDBOX_DIR"
fi

$CABAL configure --enable-tests || exit 1
$CABAL build || exit 1
$CABAL test || exit 1
