#!/bin/sh

# This script will compile faslpath and run the tests.

cd `dirname $0`

ccl -n -b --load compile-faslpath.lisp --eval '(quit)'

ccl -n -b \
    --load faslpath \
    --load impl \
    --load tests \
    --eval '(faslpath.tests:run-tests)' \
    --eval '(quit)'
