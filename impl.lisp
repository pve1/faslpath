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

;;;; Implementation specific parts go here.

(defpackage #:faslpath.impl
  (:use #:cl)
  (:export ;; Needed by faslpath.extras
           #:getenv 
           #:save-lisp-and-die

           ;; Needed by faslpath.tests
           #:run-program))

(in-package :faslpath.impl)

;;;; SBCL 

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

#+sbcl
(defun faslpath.impl:getenv (environment-variable)
  (sb-posix:getenv environment-variable))

#+sbcl
(defun run-program (program-name args)
  (sb-ext:run-program program-name args
                      :search t))

#+sbcl
(defun save-lisp-and-die (core-name)
  (sb-ext:save-lisp-and-die core-name))



;;;; CLISP

#+clisp
(defun getenv (environment-variable)
  (ext:getenv environment-variable))

#+clisp
(defun run-program (program-name args)
  (ext:run-program program-name :arguments args))

#+clisp
(defun save-lisp-and-die (core-name)
  (ext:saveinitmem core-name))

;;;; CCL

#+ccl
(defun getenv (environment-variable)
  (ccl:getenv environment-variable))

#+ccl
(defun run-program (program-name args)
  (ccl:run-program program-name args))

#+ccl
(defun save-lisp-and-die (core-name)
  (ccl:save-application core-name))



;;;; LW

;;;; ACL

;;;; ...