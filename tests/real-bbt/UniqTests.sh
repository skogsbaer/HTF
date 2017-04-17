#!/bin/bash

source $(dirname $0)/lib

rm -f .HTF/TestHTF.history

run_test UniqTests1.hs
check_success
check_counts 1 1 0 0 0 0 0

run_test UniqTests2.hs
check_success
check_counts 1 1 0 0 0 0 0
