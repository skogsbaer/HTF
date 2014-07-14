#!/bin/bash

source $(dirname $0)/lib

run_test MaxCurTime.hs
check_success
check_counts 2 2 0 0 0 0 0

run_test MaxCurTime.hs --max-cur-ms=10 --timeout-is-success
check_success
check_timeout 2
check_counts 2 2 0 0 0 2 0

run_test MaxCurTime.hs --max-cur-ms=10
check_fail
check_timeout 2
check_counts 2 0 0 0 2 2 0

run_test MaxCurTime.hs verySlow
check_success
check_timeout 0
check_counts 1 1 0 0 0 0 1

run_test MaxCurTime.hs verySlow --max-cur-ms=10
check_fail
check_timeout 1
check_counts 1 0 0 0 1 1 1
