#!/bin/sh

# This script will compile faslpath and run the tests.

cd `dirname $0`
rm *.fasl

sbcl --disable-debugger --no-userinit --load compile-faslpath.lisp --eval '(quit)'

sbcl --no-userinit \
     --disable-debugger \
     --load faslpath.fasl \
     --load impl.fasl \
     --load tests.fasl \
     --eval '(faslpath.tests:run-tests)' \
     --eval '(quit)'
