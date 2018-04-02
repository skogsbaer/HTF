#!/bin/bash

if [ "$1" == "--help" ]; then
    echo "Checks that HTF compiles against all resolvers mentioned in .travis.yml"
    exit 1
fi

YAMLS=$(grep '^- HTF_TRAVIS_STACK_ARGS' .travis.yml | sed 's/^.*="--stack-yaml //g; s/"$//g')
TOP=$(cd $(dirname ${BASH_SOURCE[0]})/.. > /dev/null && pwd -P)

if [ -z "$TRAVIS_BUILD_DIR" ]; then
    export TRAVIS_BUILD_DIR="$TOP"
fi

for yaml in $YAMLS; do
    eval yaml="$yaml" # evaluate variables contained in $yaml
    echo >&2 "Checking with $yaml ..."
    export HTF_TRAVIS_STACK_ARGS="--stack-yaml $yaml"
    stack $HTF_TRAVIS_STACK_ARGS test
    ecode=$?
    if [ $ecode -ne 0 ]; then
        echo >&2 "Check for $yaml failed!"
        exit 1
    fi
done
