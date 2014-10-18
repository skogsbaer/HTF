#!/bin/bash

SANDBOX_FILE="cabal.sandbox.config"
SANDBOX_DIR=".cabal-sandbox"

set -e
if [ -e "$SANDBOX_FILE"  ]
then
    have_sandbox=yes
    cabal sandbox hc-pkg unregister HTF || true
else
    have_sandbox=no
    ghc-pkg unregister HTF || true
fi

cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test || exit 1
cabal install || exit 1

cd sample

if [ "$have_sandbox" == "yes" ]
then
    rm -f cabal.sandbox.config
    cabal sandbox init --sandbox ../"$SANDBOX_DIR"
fi

cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test || exit 1
