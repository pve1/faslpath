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

(load (merge-pathnames "split-sequence.lisp" *load-truename*))
(load (merge-pathnames "tools.lisp" *load-truename*))
(load (merge-pathnames "empty.lisp" *load-truename*))
(load (merge-pathnames "loader.lisp" *load-truename*))
(load (compile-file (merge-pathnames "impl.lisp" *load-truename*)))
(load (compile-file (merge-pathnames "tests.lisp" *load-truename*)))
(load (compile-file (merge-pathnames "extras.lisp" *load-truename*)))

(let ((faslpath.loader:*faslpath* 
       (list (make-pathname :directory
                            (butlast (pathname-directory *load-truename*)))))
  
      (*default-pathname-defaults* 
       (make-pathname :directory (pathname-directory *load-truename*))))
  
  #-ccl
  (faslpath.loader:compile-package :faslpath.loader t)
  
  ;; Concatenating fasls doesn't seem to work on ccl, so we use the
  ;; loader instead.
  #+ccl
  (faslpath.loader:compile-package :faslpath.loader t t)

  (faslpath.tools:concatenate-binary-files 
   (list (faslpath::fasl-file "split-sequence")
         (faslpath::fasl-file "empty")
         (faslpath::fasl-file "tools")
         (faslpath::fasl-file "loader"))
   (faslpath::fasl-file "faslpath"))

  ;; Copy to "faslpath"
  #+ccl
  (faslpath.tools:concatenate-binary-files 
   (list (faslpath::fasl-file "loader-loader"))
   (faslpath::fasl-file "faslpath")))
