#!/bin/sh

# This script will compile faslpath and run the tests.

cd `dirname $0`
rm *.fas

clisp -on-error abort -norc \
      -x '(load "compile-faslpath.lisp")'

clisp -on-error abort -norc \
      -x '(load "faslpath.fas")' \
      -x '(load "impl.fas")' \
      -x '(load "tests.fas")' \
      -x '(faslpath.tests:run-tests)'

