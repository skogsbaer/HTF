#!/bin/sh

case "$1" in
  *bbt/should_fail/ok1.x)
    exit 1
  ;;
  *bbt/should_fail/ok2.x)
    echo x 
    echo y 1>&2
    exit 2
  ;;
  *bbt/should_fail/not_ok_because_succeeds.x)
    exit 0
  ;;
  *bbt/should_fail/not_ok_because_stdout1.x)
    exit 1
  ;;
  *bbt/should_fail/not_ok_because_stdout2.x)
    echo x
    exit 1
  ;;
  *bbt/should_fail/not_ok_because_stderr1.x)
    exit 1
  ;;
  *bbt/should_fail/not_ok_because_stderr2.x)
    echo x 1>&2
    exit 1
  ;;

  *bbt/should_pass/ok1.x)
    exit 0
  ;;
  *bbt/should_pass/ok2.x)
    echo x 
    echo y 1>&2
    exit 0
  ;;
  *bbt/should_pass/stdin_ok.x)
    cat
    exit 0
  ;;
  *bbt/should_pass/not_ok_because_fails.x)
    exit 6
  ;;
  *bbt/should_pass/not_ok_because_stdout1.x)
    exit 0
  ;;
  *bbt/should_pass/not_ok_because_stdout2.x)
    echo x
    exit 0
  ;;
  *bbt/should_pass/not_ok_because_stderr1.x)
    exit 0
  ;;
  *bbt/should_pass/not_ok_because_stderr2.x)
    echo x 1>&2
    exit 0
  ;;
  *bbt/Verbose/not_ok_because_stdout1.x)
    exit 0
  ;;
  *)
    echo "$0: Illegal failname: <$1>"
    exit 127
  ;;
esac
