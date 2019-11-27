#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Checks that HTF compiles against all resolvers mentioned in .travis.yml"
    exit 1
fi

set -e

TOP=$(cd $(dirname ${BASH_SOURCE[0]})/.. > /dev/null && pwd -P)
TRAVIS="$TOP/.travis.yml"
if [ ! -e "$TRAVIS" ]; then
    echo "$TRAVIS does not exist!"
    exit 1
fi

YAMLS=$(grep '^- HTF_TRAVIS_STACK_ARGS' "$TRAVIS" | sed 's/^.*="--stack-yaml //g; s/"$//g')

if [ -z "$TRAVIS_BUILD_DIR" ]; then
    export TRAVIS_BUILD_DIR="$TOP"
fi

for yaml in $YAMLS; do
    echo >&2
    eval yaml="$yaml" # evaluate variables contained in $yaml
    echo >&2 "Checking with $yaml ..."
    export HTF_TRAVIS_STACK_ARGS="--stack-yaml $yaml"
    stack $HTF_TRAVIS_STACK_ARGS build
    ecode=$?
    if [ $ecode -ne 0 ]; then
        echo >&2 "Build for $yaml failed!"
        exit 1
    fi
    stack $HTF_TRAVIS_STACK_ARGS test
    ecode=$?
    if [ $ecode -ne 0 ]; then
        echo >&2 "Test for $yaml failed!"
        exit 1
    fi
done
