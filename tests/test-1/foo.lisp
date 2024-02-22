
(defpackage #:faslpath.tests.test-1.foo
  (:use :cl
        :faslpath.tests.test-1.bar
        :faslpath.tests.test-2.foo
        :faslpath.tests.test-2.bar)

  (:export #:foo-1))

(in-package :faslpath.tests.test-1.foo)
