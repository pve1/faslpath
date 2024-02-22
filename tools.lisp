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


(defpackage #:faslpath.tools
  (:use #:cl)
  (:export #:concatenate-binary-files
           #:remove-package
           #:clear-package
           #:map-tree
           #:mapc-tree
           #:tree-postorder
           #:prune-tree))

(in-package :faslpath.tools)

(defun concatenate-binary-files (files output-file)
  (let ((buf (make-array 10000 
                         :fill-pointer t
                         :element-type '(unsigned-byte 8))))
    (with-open-file (out output-file 
                         :if-exists :supersede
                         :direction :output 
                         :element-type '(unsigned-byte 8))
      (dolist (in-file files)
        (setf (fill-pointer buf) 10000)
        (with-open-file (in in-file :element-type '(unsigned-byte 8))
          (loop :for read = (read-sequence buf in)
             :until (= 0 read)
             :do (setf (fill-pointer buf) read)             
             (write-sequence buf out)))))))

(defun invoke-main (package)
  (let ((s (find-symbol "MAIN" package)))
    (if s
        (funcall s)
        (error "Symbol MAIN not found in the ~A package" package))))

(defun clear-package (package-name)
  (let ((p (find-package package-name)))
    (when p
      (dolist (used (package-use-list p))
        (unuse-package used p)) 
      (do-symbols (s p)
        (unintern s p)))))

(defun remove-package (p)
  (ignore-errors
    (clear-package p)
    (delete-package p)))

(defun map-tree (tree function)
  (if (null tree)
      nil
      (typecase (first tree)
        (atom (cons (funcall function (first tree))
                    (map-tree (rest tree) function)))
        (list (cons (map-tree (first tree) function)
                    (map-tree (rest tree) function))))))

(defun mapc-tree (tree function)
  (if (null tree)
      nil
      (typecase (first tree)
        (atom (funcall function (first tree))
              (mapc-tree (rest tree) function))
        (list (mapc-tree (first tree) function)
              (mapc-tree (rest tree) function)))))


(defun prune-tree (tree list-predicate atom-predicate) 
  (labels ((walk (tree)
             (typecase tree
               (null nil)
               (atom (if (funcall atom-predicate tree)
                         tree
                         nil))
               (list                  
                (if (funcall list-predicate tree)
                    (cons (walk (first tree))
                          (walk (rest tree)))
                    nil)))))
    (walk tree)))

(defun tree-postorder (tree)
  (let ((nodes))
    (mapc-tree tree (lambda (x) 
                      (push x nodes)))
    nodes))

