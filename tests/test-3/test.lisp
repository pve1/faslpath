(defpackage #:faslpath.tests.test-3.test
  (:use #:cl
        :faslpath.tests.test-1.foo
        :faslpath.tests.test-2.foo))

(in-package :faslpath.tests.test-3.test)

(dolist (p '(:faslpath.tests.test-1.foo
             :faslpath.tests.test-2.foo))
  (do-external-symbols (s p)
    (export s)))

(defun main ()
  (print 'HELLO)
  (terpri))
