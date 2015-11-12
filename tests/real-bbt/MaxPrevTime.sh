#!/bin/bash

source $(dirname $0)/lib

run_test MaxPrevTime.hs
check_success
check_counts 2 2 0 0 0 0 0

run_test MaxPrevTime.hs --max-prev-ms=10
check_success
check_counts 1 1 0 0 0 0 1
