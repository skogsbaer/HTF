#!/bin/bash

source $(dirname $0)/lib

rm -f .HTF/TestHTF.history

run_test Repeat.hs --repeat 1
check_success
check_counts 1 1 0 0 0 0 0

run_test Repeat.hs --repeat 2
check_fail
check_counts 1 0 0 1 0 0 0