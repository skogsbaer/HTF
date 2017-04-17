#!/bin/bash

source $(dirname $0)/lib

rm -f .HTF/TestHTF.history

run_test PrevFactor.hs
check_success
check_counts 1 1 0 0 0 0 0

run_test PrevFactor.hs SLOW --prev-factor=2 --timeout-is-success
check_success
check_timeout
check_counts 1 1 0 0 0 1 0

run_test PrevFactor.hs SLOW --prev-factor=2
check_fail
check_timeout
check_counts 1 0 0 0 1 1 0
