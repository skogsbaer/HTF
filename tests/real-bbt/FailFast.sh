#!/bin/bash

source $(dirname $0)/lib

run_test FailFast.hs --fail-fast
check_fail
check_counts 1 0 0 1 0 0 0
