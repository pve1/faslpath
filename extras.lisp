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

(defpackage #:faslpath.extras
  (:use #:cl
        #:faslpath.loader
        #:faslpath.impl)
  (:export #:make-package-lisp-core))

(in-package :faslpath.extras)

(defvar *faslpath-core-directory*)

(defvar *faslpath-core-directory-env-variable*
  "FASLPATH_CORE_DIR")

(defun default-faslpath-core-dir ()
  (if (boundp '*faslpath-core-directory*)
      *faslpath-core-directory*
      (merge-pathnames #P".faslpath-cores/" (user-homedir-pathname))))

(defun make-package-lisp-core (package-name &optional core-path) ;; /foo/bar/blah.core
  ;; Figure out where to save the core.
  (let* ((*default-pathname-defaults*
          (let ((env (getenv *faslpath-core-directory-env-variable*)))
            (if env
                (pathname (concatenate 'string env "/"))
                (default-faslpath-core-dir))))

         (core-name (if core-path
                        core-path
                        (merge-pathnames
                         (make-pathname
                          :name (faslpath.loader::resolve-package package-name)
                          :type "core")))))
    (when core-name
      (ensure-directories-exist *default-pathname-defaults*)
      (save-lisp-and-die core-name))))
