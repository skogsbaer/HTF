#!/bin/bash

source $(dirname $0)/lib

rm -f .HTF/TestHTF.history

function get_line_no()
{
    grep -n '\[TEST\] SortByPrevTime:'"$1" "$OUT" | gawk -F : '{print $1}'
}

run_test SortByPrevTime.hs
check_success
line_slow1=$(get_line_no slow)
line_fast1=$(get_line_no fast)
if [ $line_slow1 -ge $line_fast1 ]
then
    do_fail "1st run: line_slow1=$line_slow1, line_fast1=$line_fast1"
fi

run_test SortByPrevTime.hs --sort-by-prev-time
check_success
line_slow2=$(get_line_no slow)
line_fast2=$(get_line_no fast)
if [ $line_slow2 -lt $line_fast2 ]
then
    do_fail "2nd run: line_slow2=$line_slow2, line_fast2=$line_fast2"
fi
