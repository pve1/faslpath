;;;; Copyright 2009 Peter von Etter

;;;; This file is part of Faslpath.

;;;; Faslpath is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; Faslpath is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with Faslpath.  If not, see
;;;; <http://www.gnu.org/licenses/>.

;;;; TODO: clean up

(defpackage #:faslpath.tests
  (:use #:cl
        :faslpath.tools
        :faslpath.loader
        :faslpath.impl)
  (:export #:run-tests)
  (:import-from :faslpath.loader
                #:*faslpath-trace*
                #:fasl-file
                #:delete-package-tree))
 
(in-package :faslpath.tests)

(defparameter *test-packages* 
  '(:faslpath.tests.test-5.linear
    :faslpath.tests.test-4.qwe
    :faslpath.tests.test-4.bar.bar
    :faslpath.tests.test-3.test
    :faslpath.tests.test-1.foo
    :faslpath.tests.test-2.foo
    :faslpath.tests.test-2.bar
    :faslpath.tests.test-2.bar))

(defun delete-test-files ()
  (dolist (f '("tests/test-1/bar"
               "tests/test-1/foo"
               "tests/test-1/foo-loader"
               "tests/test-2/bar"
               "tests/test-2/foo"
               "tests/test-3/test"
               "tests/test-4/qwe"
               "tests/test-4/xyzzy"
               "tests/test-4/xyzzy-2"
               "tests/test-4/foo/foo"
               "tests/test-4/bar/bar"
               "tests/test-4/bar/asdf"
               "tests/test-4/bar/zxcv"
               "tests/test-5/linear"
               "tests/test-5/a"
               "tests/test-5/b"
               "tests/test-5/c")) 
    (ignore-errors (delete-file (fasl-file f)))
    #+nil
    (handler-case (delete-file (fasl-file f))
      (error (x) (describe x)))) 
    (ignore-errors (delete-file "tests/test-1/foo-loader.lisp")))

(defun delete-test-packages ()
  (dolist (p *test-packages*)
    (remove-package p)))

(defun fresh-test-context ()
  (delete-test-files)
  (delete-test-packages))

(defun touch-file (file)
  (run-program "touch" (list (namestring (truename file)))))
   
(defun inform (msg)
  (format t ">> Testing: ~A~%" (format nil msg)))

(defun test-1 ()
  (let* ((*faslpath* 
          (list 
           (pathname
            (make-pathname 
             :directory 
             (butlast (pathname-directory *default-pathname-defaults*))))))
         (deps)
         (compile-trace)
         (*load-verbose* t))

    (inform "Dependency tree")
    (setf deps 
          (mapcar 'faslpath::pkg-name
                  (tree-postorder
                   (faslpath.loader::compute-package-dependencies/all/pkg
                    :faslpath.tests.test-1.foo))))
    
    (assert (equal deps
                   '(:CL
                     :FASLPATH.TESTS.TEST-2.BAR
                     :CL
                     :FASLPATH.TESTS.TEST-2.BAR
                     :CL
                     :FASLPATH.TESTS.TEST-2.FOO
                     :CL
                     :FASLPATH.TESTS.TEST-1.BAR
                     :CL
                     :FASLPATH.TESTS.TEST-1.FOO)))

    (fresh-test-context)
    
    ;; Compile package

    (inform "Basic compile, no fasls present.")

    (compile-package :faslpath.tests.test-1.foo)

    (assert (probe-file (fasl-file "tests/test-1/bar")))
    (assert (probe-file (fasl-file "tests/test-1/foo")))
    (assert (probe-file (fasl-file "tests/test-2/bar")))
    (assert (probe-file (fasl-file "tests/test-2/foo")))

    
    (inform "Update a file and recompile.")
    
    (sleep 1)
    (touch-file "tests/test-1/bar.lisp")
    
    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*))

    (assert (equal compile-trace 
                   '((:compile :faslpath.tests.test-1.foo)
                     (:load :faslpath.tests.test-2.foo)
                     (:load :faslpath.tests.test-2.bar)
                     (:compile :faslpath.tests.test-1.bar))))

    (inform "Update and recompile a file (as in C-c C-k), ~
             and recompile the package.")
    
    (sleep 1)
    (touch-file "tests/test-1/bar.lisp")
    (compile-file "tests/test-1/bar.lisp")

    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*))
    
    (assert (equal compile-trace 
                   '((:compile :faslpath.tests.test-1.foo)
                     (:load :faslpath.tests.test-2.foo)
                     (:load :faslpath.tests.test-2.bar)
                     (:load :faslpath.tests.test-1.bar))))

    (inform "Update and recompile a file (as in C-c C-k), ~
             and recompile the package.")

    (sleep 1)
    (touch-file "tests/test-2/bar.lisp")
    (compile-file "tests/test-2/bar.lisp")

    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*))
    
    (assert (equal compile-trace 
                   '((:compile :faslpath.tests.test-1.foo)
                     (:compile :faslpath.tests.test-2.foo)
                     (:load :faslpath.tests.test-2.bar)
                     (:load :faslpath.tests.test-1.bar))))

    (inform "Recompile a file (as in C-c C-k), and recompile the package. ~
             Should not cause compilation of any files.")
    
    (sleep 1)
    (compile-file "tests/test-2/bar.lisp")
    
    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*))
    
    (assert (equal compile-trace 
                   '((:load :faslpath.tests.test-1.foo)
                     (:load :faslpath.tests.test-2.foo)
                     (:load :faslpath.tests.test-2.bar)
                     (:load :faslpath.tests.test-1.bar))))


    (inform "Update every file and recompile.")
    
    (sleep 1)
    (touch-file "tests/test-1/foo.lisp")
    (touch-file "tests/test-1/bar.lisp")
    (touch-file "tests/test-2/foo.lisp")
    (touch-file "tests/test-2/bar.lisp")
    
    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*))
    
    (assert (equal compile-trace 
                   '((:compile :faslpath.tests.test-1.foo)
                     (:compile :faslpath.tests.test-2.foo)
                     (:compile :faslpath.tests.test-2.bar)
                     (:compile :faslpath.tests.test-1.bar))))
    

    (inform "Recompile up-to-date (and loaded) package with ~
             tabula-rasa and make-loader.")
    
    (intern "O-HAI" :faslpath.tests.test-1.foo)
    (intern "O-HAI" :faslpath.tests.test-1.bar)
    (intern "O-HAI" :faslpath.tests.test-2.foo)
    (intern "O-HAI" :faslpath.tests.test-2.bar)
    
    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-1.foo t t)
      (setf compile-trace *faslpath-trace*))
    
    ;; Symbols should be gone after tabula-rasa
    (assert (not (or (find-symbol "O-HAI" :faslpath.tests.test-1.foo)
                     (find-symbol "O-HAI" :faslpath.tests.test-1.bar)
                     (find-symbol "O-HAI" :faslpath.tests.test-2.foo)
                     (find-symbol "O-HAI" :faslpath.tests.test-2.bar))))
    
    (assert (equal compile-trace 
                   '((:load :faslpath.tests.test-1.foo)
                     (:load :faslpath.tests.test-2.foo)
                     (:load :faslpath.tests.test-2.bar)
                     (:load :faslpath.tests.test-1.bar))))

    (assert (probe-file 
             (faslpath.loader::package-loader-name
              :faslpath.tests.test-1.foo)))
    
    (assert (probe-file 
             (merge-pathnames faslpath.loader::*fasl-type* 
                              (faslpath.loader::package-loader-name
                               :faslpath.tests.test-1.foo))))

    (inform "Compile from scratch")

    (let ((*faslpath-trace* nil))
      (compile-package-from-scratch :faslpath.tests.test-1.foo)
      (setf compile-trace *faslpath-trace*)) 

    (assert (equal compile-trace 
                   '((:COMPILE :FASLPATH.TESTS.TEST-1.FOO)
                     (:COMPILE :FASLPATH.TESTS.TEST-2.FOO)
                     (:COMPILE :FASLPATH.TESTS.TEST-2.BAR)
                     (:COMPILE :FASLPATH.TESTS.TEST-1.BAR))))
        
    (inform "Check for 'facade' breakage with tabula rasa.")
    ;; Need to apply clear-package to the entire dependency tree
    ;; before loading anything
    
    (fresh-test-context)
    (compile-package :faslpath.tests.test-3.test t)
    (compile-package :faslpath.tests.test-3.test t)


    (inform "Load package with tabula-rasa")
    
    (intern "O-HAI" :faslpath.tests.test-1.foo)
    (intern "O-HAI" :faslpath.tests.test-1.bar)
    (intern "O-HAI" :faslpath.tests.test-2.foo)
    (intern "O-HAI" :faslpath.tests.test-2.bar)

    (let ((*faslpath-trace* nil))
      (load-package :faslpath.tests.test-3.test t)
      (setf compile-trace *faslpath-trace*))
    
    (assert 
     (equal compile-trace
            '((:LOAD :FASLPATH.TESTS.TEST-3.TEST) 
              (:LOAD :FASLPATH.TESTS.TEST-1.FOO)
              (:LOAD :FASLPATH.TESTS.TEST-2.FOO)
              (:LOAD :FASLPATH.TESTS.TEST-2.BAR)
              (:LOAD :FASLPATH.TESTS.TEST-1.BAR))))    
    
    ;; Symbols should be gone after tabula-rasa
    (assert (not (or (find-symbol "O-HAI" :faslpath.tests.test-1.foo)
                     (find-symbol "O-HAI" :faslpath.tests.test-1.bar)
                     (find-symbol "O-HAI" :faslpath.tests.test-2.foo)
                     (find-symbol "O-HAI" :faslpath.tests.test-2.bar))))
    
    
    (inform "Load an already loaded package.")

    (let ((*faslpath-trace* nil))
      (load-package :faslpath.tests.test-3.test) 
      (setf compile-trace *faslpath-trace*)) 

    (assert (equal compile-trace nil))
    
    
    (inform "Load partial package tree.")

    (fresh-test-context)
    (compile-package :faslpath.tests.test-3.test)
    (delete-package-tree :faslpath.tests.test-3.test)

    (load-package :faslpath.tests.test-2.bar)
    (load-package :faslpath.tests.test-1.bar)

     (let ((*faslpath-trace* nil))
       (load-package :faslpath.tests.test-3.test) 
       (setf compile-trace *faslpath-trace*))

    (assert 
     (equal compile-trace
            '((:LOAD :FASLPATH.TESTS.TEST-3.TEST)
              (:LOAD :FASLPATH.TESTS.TEST-1.FOO)
              (:LOAD :FASLPATH.TESTS.TEST-2.FOO))))
        
    
    (fresh-test-context)
    
    (inform "Quoted list dependency tree.")

    (assert
     (equal 
      (faslpath.loader::compute-package-dependencies/all 
       :faslpath.tests.test-4.qwe)      
      '(:FASLPATH.TESTS.TEST-4.QWE
        ("xyzzy" ("xyzzy-2") ("foo/foo")) ("foo/foo")
        (:FASLPATH.TESTS.TEST-4.BAR.BAR
         ("asdf" ("zxcv")) ("zxcv"))))) 

    
    (inform "Quoted list compile-package.")

    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-4.qwe)
      (setf compile-trace *faslpath-trace*)) 
    
    (assert (equal compile-trace
                   '((:COMPILE :FASLPATH.TESTS.TEST-4.QWE)
                     (:COMPILE :FASLPATH.TESTS.TEST-4.BAR.BAR)
                     (:COMPILE "asdf")
                     (:COMPILE "zxcv")
                     (:COMPILE "xyzzy")
                     (:COMPILE "foo/foo")
                     (:COMPILE "xyzzy-2"))))    
    
    (inform "Quoted list update one file and recompile.")
    
    (sleep 1)
    (touch-file "tests/test-4/foo/foo.lisp")
    (compile-file "tests/test-4/foo/foo.lisp")

    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-4.qwe)
      (setf compile-trace *faslpath-trace*)) 
    
    (assert 
     (equal compile-trace
            '((:COMPILE :FASLPATH.TESTS.TEST-4.QWE)
              (:LOAD :FASLPATH.TESTS.TEST-4.BAR.BAR)
              (:LOAD "asdf")
              (:LOAD "zxcv")
              (:COMPILE "xyzzy")
              (:LOAD "foo/foo")
              (:LOAD "xyzzy-2"))))   
    
    
    (inform "Quoted list load tabula rasa.")

    (let ((*faslpath-trace* nil))
      (load-package :faslpath.tests.test-4.qwe t)
      (setf compile-trace *faslpath-trace*)) 
    
    (assert 
     (equal compile-trace
            '((:LOAD :FASLPATH.TESTS.TEST-4.QWE) 
              (:LOAD :FASLPATH.TESTS.TEST-4.BAR.BAR)
              (:LOAD "asdf")
              (:LOAD "zxcv")
              (:LOAD "xyzzy")
              (:LOAD "foo/foo")
              (:LOAD "xyzzy-2"))
            
            #+nil
            '((:LOAD :FASLPATH.TESTS.TEST-4.QWE)
              (:LOAD "xyzzy")
              (:LOAD "xyzzy-2")
              (:LOAD "foo/foo")
              (:LOAD :FASLPATH.TESTS.TEST-4.BAR.BAR)
              (:LOAD "asdf")
              (:LOAD "zxcv")))) 


    (inform "Quoted list partial load.")
    
    (fresh-test-context)
    (compile-package :faslpath.tests.test-4.qwe)
    (delete-package-tree :faslpath.tests.test-4.qwe)

    (load-package :faslpath.tests.test-4.bar.bar)

    (let ((*faslpath-trace* nil))
      (load-package :faslpath.tests.test-4.qwe) 
      (setf compile-trace *faslpath-trace*))
    
    (assert 
     (equal compile-trace
            '((:LOAD :FASLPATH.TESTS.TEST-4.QWE)
              (:LOAD "xyzzy") 
              (:LOAD "foo/foo")
              (:LOAD "xyzzy-2"))))

    
    (inform "check-filename")
    
    (faslpath.loader::check-filename 
     '(defpackage :faslpath.loader (:use :foo))
     "/foo/bar/faslpath/faslpath/loader.lisp")

    
    (inform "Linear dependency compile.")
    
    (fresh-test-context)

    (let ((*faslpath-trace* nil))
      (compile-package :faslpath.tests.test-5.linear) 
      (setf compile-trace *faslpath-trace*))
    
    (assert 
     (equal compile-trace
            '((:COMPILE :FASLPATH.TESTS.TEST-5.LINEAR)
              (:COMPILE "c")
              (:COMPILE "b")
              (:COMPILE "a"))))    
    
    (inform "Linear dependency load.")
    
    (delete-package-tree :faslpath.tests.test-5.linear)

    (let ((*faslpath-trace* nil))
      (load-package :faslpath.tests.test-5.linear) 
      (setf compile-trace *faslpath-trace*))
    
    (assert 
     (equal compile-trace
            '((:LOAD :FASLPATH.TESTS.TEST-5.LINEAR)
              (:LOAD "c")
              (:LOAD "b")
              (:LOAD "a"))))

    (fresh-test-context)

    (inform "Linear dependency compile-from-scratch.")
    
    (let ((*faslpath-trace* nil))
      (compile-package-from-scratch :faslpath.tests.test-5.linear) 
      (setf compile-trace *faslpath-trace*))
    
    (assert 
     (equal compile-trace
            '((:COMPILE :FASLPATH.TESTS.TEST-5.LINEAR)
              (:COMPILE "c")
              (:COMPILE "b")
              (:COMPILE "a"))))))

    
(let ((c *load-truename*))
  (defun run-tests ()
    (let ((*default-pathname-defaults* 
           (pathname (directory-namestring c))))
      (test-1)
      nil)))

(defun main ()
  (run-tests))
