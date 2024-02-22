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


(defpackage #:faslpath.loader
  (:use #:cl
       #:faslpath.split
-sequence ;; Bundled for convenience
        #:faslpath.tools
        #:faslpath.empty)
  (:nicknames #:fap #:faslpath)
  (:export #:compile-package
           #:load-package
           #:*faslpath*
           #:compile-package-from-scratch))

(in-package :faslpath.loader)

;;;; Implementation specific

#+sbcl
(defparameter *fasl-type*
  (make-pathname :type "fasl"))

#+sbcl
(defparameter *source-type*
  (make-pathname :type "lisp"))

#+clisp
(defparameter *fasl-type*
  (make-pathname :type "fas"))

#+clisp
(defparameter *source-type*
  (make-pathname :type "lisp"))

#+ccl
(defparameter *fasl-type*
  ccl:*.fasl-pathname*)

#+ccl
(defparameter *source-type*
  (make-pathname :type "lisp"))

;;;; Implementation specific end

(defvar *faslpath* nil
  "A list of directory pathnames that indicates where the root package
  directories are located.")

(defvar *loaded-files*)

(defvar *faslpath-trace*)
(defvar *faslpath-file-trace*)

(defun compile-needed? (file)
  "Returns t if the source of FILE is newer than the object."
  (let ((source (merge-pathnames *source-type* file))
        (object (merge-pathnames *fasl-type* file)))
    (cond ((and (probe-file source)
                (probe-file object))
           (< (file-write-date object)
              (file-write-date source)))
          (t t))))

(defun compile-needed?* (source-timestamp object-timestamp)
  (cond ((and source-timestamp (not object-timestamp))
         t)

        ((and source-timestamp
              object-timestamp
              (< object-timestamp source-timestamp))
         t)

        (t nil)))

(defun compile-when-needed (file)
  "Compiles a file if needed."
  (let ((source (merge-pathnames *source-type* file))
        (object (merge-pathnames *fasl-type* file)))
    (if (compile-needed? source)
        (compile-file source)
        object)))

(defun current-library-paths ()
  *faslpath*)

(defun probe-for-defpackage-form (file)
  (let ((dp-form (with-open-file (fs file)
                   (read fs))))
    dp-form))

(defstruct pkg name filename source-timestamp object-timestamp)
(defstruct (file (:include pkg)))

(defun %compute-package-dependencies (defpackage-form)
  (when (listp defpackage-form)
    (case (first defpackage-form)
      (defpackage
          (destructuring-bind (defpackage package-name &rest options)
              defpackage-form
            (declare (ignore package-name defpackage))
            (let ((use-list (assoc :use options)))
              (rest use-list))))
      (quote (second defpackage-form)))))

(defun %parse-package-dependencies (defpackage-form)
  (when (listp defpackage-form)
    (case (first defpackage-form)
      (defpackage
          (destructuring-bind (defpackage package-name &rest options)
              defpackage-form
            (declare (ignore package-name defpackage))
            (let ((use-list (assoc :use options)))
              (mapcar (lambda (x)
                        (make-pkg :name x))
                      (rest use-list)))))
      (quote
       (mapcar (lambda (x)
                 (typecase x
                   (symbol (make-pkg :name x))
                   (string (make-file :name x))))
               (second defpackage-form))))))

(defun split-package-name (package-name)
  (split-sequence #\. (string-downcase package-name)))

(defun package-file-name (package-name)
  (let ((namestring (car (last (split-package-name package-name)))))
    (merge-pathnames namestring *source-type*)))

(defun resolve-package (package-designator)
  (typecase package-designator
    (symbol (string-downcase package-designator))
    (t package-designator)))

(defun check-filename (defpackage-form filename)
  (when (listp defpackage-form)
    (case (first defpackage-form)
      (defpackage
          (let* ((package-name (resolve-package (second defpackage-form)))
                 (split-name (split-package-name package-name))
                 (package-dirs (butlast split-name))
                 (filename-dirs (rest (pathname-directory filename)))
                 (filename-dirs-length (length filename-dirs))

                 ;; Handle case where package-name has no dots separately.
                 (i (if package-dirs
                        (search package-dirs filename-dirs
                                :test #'string=
                                :from-end t)
                        (length filename-dirs)))
                 (path-good? (when i
                               (= (length package-dirs)
                                  (- filename-dirs-length i))))
                 (file-name-good? (equal (pathname-name filename)
                                         (car (last split-name)))))

            (unless (and path-good? file-name-good?)
              (error "Package-name ~A doesn't match file name ~A."
                     package-name
                     filename)))))))

(defun package-directory (package-name)
  (let ((dirs (butlast (split-package-name package-name))))
    (make-pathname :directory (cons :relative dirs))))

(defun package-relative-path (package-name &optional (type *source-type*))
  (let* ((path (split-package-name package-name))
         (dirs (cons :relative (butlast path))))
    (make-pathname :directory dirs
                   :name (car (last path))
                   :type (pathname-type type))))

(defun find-package-in-library (library-path package-name
                                &optional (type *source-type*))
  (probe-file (merge-pathnames (package-relative-path package-name type)
                               library-path)))

(defun package-root (path package-symbol)
  (let* ((chunks (split-package-name (resolve-package package-symbol)))
         (dir (pathname-directory path))
         (pos (search (butlast chunks) dir :test #'equal)))
    (unless pos
      (error "Bad package ~A in path ~S" package-symbol path))
    (make-pathname :directory (subseq dir 0 pos))))


;;;;

#+nil
(defun pkg-equal (pkg-1 pkg-2)
  (declare (pkg pkg-1 pkg-2))
  (or (let ((name-1 (pkg-name pkg-1))
            (name-2 (pkg-name pkg-2)))
        (and name-1
             name-2
             (eq name-1 name-2)))
      (let ((filename-1 (pkg-filename pkg-1))
            (filename-2 (pkg-filename pkg-2)))
        (and filename-1
             filename-2
             (equal filename-1 filename-2)))))

(defun pkg-equal (pkg-1 pkg-2)
  (declare (pkg pkg-1 pkg-2))
  (if (and (file-p pkg-1)
           (file-p pkg-2))
      (let ((name-1 (pkg-filename pkg-1))
            (name-2 (pkg-filename pkg-2)))
        (and name-1
             name-2
             (equal name-1 name-2)))
      (let ((name-1 (pkg-name pkg-1))
            (name-2 (pkg-name pkg-2)))
        (and name-1
             name-2
             (eq name-1 name-2)))))

(defun fasl-file (file)
  (merge-pathnames *fasl-type* file))

(defun source-file (file)
  (merge-pathnames *source-type* file))

(defun find-package-file (package-symbol &optional (type *source-type*))
  (labels ((find-it (package-name libs)
             (if (null libs)
                 nil
                 (let ((try (find-package-in-library (first libs)
                                                     package-name
                                                     type)))
                   (if try
                       try
                       (find-it package-name (rest libs)))))))
    (find-it (resolve-package package-symbol)
             (current-library-paths))))

(defun compute-package-dependencies/1 (package-symbol)
  (let* ((file (find-package-file package-symbol)))
    (when file
      (let ((defpackage-form (probe-for-defpackage-form file)))
        ;; (check-filename defpackage-form file)
        (%parse-package-dependencies defpackage-form)))))


;; Todo: make this a proper parameter
(defvar *relative-file-anchor*)

(defun %compute-package-dependencies/all (package-designator
                                          map-function
                                          &optional (predicate-fn #'identity)
                                          (recursive t))
  (labels ((f (file)
             (if file
                 (let* ((defpackage-form (probe-for-defpackage-form
                                          file))
                        (*relative-file-anchor* file)
                        (deps (%parse-package-dependencies defpackage-form)))
                   ;; (check-filename defpackage-form file)
                   (cons (funcall map-function
                                  package-designator
                                  file
                                  defpackage-form)
                         (if recursive
                             (mapcar (lambda (x)
                                       (%compute-package-dependencies/all
                                        x map-function predicate-fn))
                                     deps)
                             deps)))
                   (list (funcall map-function package-designator file nil)))))

    (when (funcall predicate-fn package-designator)
      (typecase package-designator

        (symbol
         (let ((file (find-package-file package-designator)))
           (f file)))

        ;; pkg-name here is the relative path to the file dependency
        (file
         (let ((file (merge-pathnames (pkg-name package-designator)
                                      *relative-file-anchor*)))
           (assert (probe-file file))
           (f file)))

        (pkg
         (let ((file (find-package-file (pkg-name package-designator))))
           (f file)))))))

(defun compute-package-dependencies/timestamps/all (package-symbol)
  (%compute-package-dependencies/all
   package-symbol
   (lambda (x y z)
     (declare (ignore z))
     (let ((source-timestamp
            (when y (file-write-date y)))
           (obj-timestamp
            (when y
              (let ((object (fasl-file y)))
                (when (probe-file object)
                  (file-write-date object))))))
       (typecase x
         (symbol
          (make-pkg
           :name x
           :filename y
           :source-timestamp source-timestamp
           :object-timestamp obj-timestamp))
         (pkg
          (setf (pkg-filename x) y)
          (setf (pkg-source-timestamp x) source-timestamp)
          (setf (pkg-object-timestamp x) obj-timestamp)
          x))))))

(defun compute-package-dependencies/all/pkg (package-symbol
                                             &optional (predicate-fn #'identity))
  (%compute-package-dependencies/all
   package-symbol
   (lambda (x y z)
     (declare (ignore z))
     (typecase x
       (symbol (make-pkg :name x :filename y))
       (pkg (setf (pkg-filename x) y)
            x)))
   predicate-fn))

;; (defun compute-package-dependencies/one (package-symbol
;;                                          &optional (predicate-fn #'identity))
;;   (%compute-package-dependencies/all
;;    package-symbol
;;    (lambda (x y z)
;;      (declare (ignore z))
;;      (typecase x
;;        (symbol (make-pkg :name x :filename y))
;;        (pkg (setf (pkg-filename x) y)
;;             x)))
;;    predicate-fn))

(defun compute-package-dependencies/all (package-symbol)
  (map-tree
   (compute-package-dependencies/all/pkg package-symbol)
   #'pkg-name))

;; (defun compute-package-dependencies/all-files (package-symbol)
;;   (let ((tree (compute-package-dependencies/all package-symbol)))
;;     (map-tree tree #'find-package-file)))

;; (defun map-package-dependencies (package-symbol function)
;;   (map-tree (compute-package-dependencies/all package-symbol)
;;             function))

;; (defun mapc-package-dependencies (package-symbol function)
;;   (mapc-tree (compute-package-dependencies/all package-symbol)
;;              function))

;; (defun apply-to-package-deps (package-symbol function)
;;   (let* ((files)
;;          (f (lambda (x)
;;               (let ((f (find-package-file x)))
;;                 (when f (push (list x f) files))))))
;;     (mapc-package-dependencies package-symbol f)
;;     (dolist (f files)
;;       (funcall function f))))

(defun reverse-package-dependencies (package-symbol)
  (tree-postorder
   (compute-package-dependencies/all package-symbol)))

;;;;

(defun package-inside (package-designator mask)
  (typecase package-designator
    (symbol
     (let ((p (split-package-name (resolve-package package-designator)))
           (m (split-package-name (resolve-package mask))))
       (and (<= (length m)
                (length p))
            (loop :for i :in m
                  :for j :in p
                  :always (equal i j)))))
    (pkg
     (package-inside (pkg-name package-designator) mask))

    (string t)))

(defun siblingsp (package-symbol-1 package-symbol-2)
  (let ((p (split-package-name (resolve-package package-symbol-1)))
        (m (split-package-name (resolve-package package-symbol-2))))
    (equal (butlast m)
           (butlast p))))

(defun squash-tree-for-load (tree)
  (let ((result))
    (labels ((walk (tr)
               (typecase tr
                 (atom (push tr result))
                 (list (walk (first tr))
                       (let ((tr* (reverse (rest tr))))
                         (dolist (x tr*)
                           (walk x)))))))
      (walk tree)
      result)))

(defun check-package (package-symbol)
  (if (find-package-file package-symbol)
      t
      (warn "Can't find package ~A in paths ~A"
            package-symbol
            (current-library-paths))))

(defun package-loader-name (package-symbol &optional (type *source-type*))
  (let* ((path (find-package-file package-symbol type))
         (file-name (car (last (split-package-name
                                (resolve-package package-symbol)))))
         (loader
          (merge-pathnames (format nil "~A-loader" file-name)
                           path)))
    loader))


;; Remember to bind *package*
(defun write-package-loader (package-symbol file-list
                             &optional loader-name)
  (let* ((loader (or loader-name
                     (package-loader-name package-symbol)))
         (*package* (find-package :faslpath.empty)))
    (with-open-file (f loader :direction :output :if-exists :supersede)
      (dolist (file file-list)
        (print `(load ,file) f)))
    loader))

;; Remember to bind *package*
(defun write-package-loader/dyn (package-symbol packages-list file-list
                                 &optional loader-name)
  (let* ((loader (or loader-name (package-loader-name package-symbol)))
         (*package* (find-package :faslpath.empty)))
    (with-open-file (f loader :direction :output :if-exists :supersede)
      (loop :for file :in file-list
            :for package :in packages-list
            :do (if (siblingsp package-symbol package)
                    (print `(load ,file) f)
                    (print `(load-package ,package) f))))
    loader))

(defun load-unless-loaded (pkg)
  (cond ((and (pkg-filename pkg)
              (symbolp (pkg-filename pkg)))
         `(unless (find-package ,(pkg-name pkg))
            (load ,(fasl-file (pkg-filename pkg)))))
        ((pkg-filename pkg)
         `(load ,(fasl-file (pkg-filename pkg))))))

(defun load-always (pkg)
  (when (pkg-filename pkg)
    `(load ,(fasl-file (pkg-filename pkg)))))

;; Remember to bind *package*
(defun write-package-loader/tree (dependency-tree
                                  loader-map-fn &optional loader-name)
  (let* ((loader (or loader-name (package-loader-name
                                  (pkg-name (car dependency-tree)))))
         (*package* (find-package :faslpath.empty))
         (handled-files))
      (with-open-file (f loader :direction :output :if-exists :supersede)
        (let ((files (squash-tree-for-load dependency-tree)))
          (dolist (file files)
            (unless (find file handled-files :test #'pkg-equal)
              (let ((x (funcall loader-map-fn file)))
                (when x
                  (print x f)
                  (push file handled-files)))))))
      loader))

(defun tabula-rasa (x)
  (when (and (not (file-p x))
             (pkg-p x)
             (pkg-filename x))
    (clear-package (pkg-name x))))

(defun do-tabula-rasa (deps)
  (map-tree deps #'tabula-rasa))

(defun compile-package-from-scratch (package-symbol &optional make-loader)
  "Unconditionally compiles and loads PACKAGE-SYMBOL and all of its dependencies."
  (let* ((loaded-files)
         (loaded-filenames)
         (*package* (find-package :cl-user))
         (f (lambda (x)
              (when (pkg-filename x)
                (unless (find x loaded-files
                              :test #'pkg-equal) ;; Only load once
                  (load (compile-file (pkg-filename x)))
                  (push x loaded-files)
                  (push (pkg-filename x) loaded-filenames)
                  (when (boundp '*faslpath-trace*)
                    (push `(:compile ,(pkg-name x)) *faslpath-trace*)))))))
    (when (check-package package-symbol)
      (let ((deps (compute-package-dependencies/all/pkg package-symbol)))
        (do-tabula-rasa deps)
        (with-compilation-unit ()
          (mapc f (squash-tree-for-load deps)))
        (when make-loader
            (compile-file
             (write-package-loader/tree
              deps
              'load-always
              (if (stringp make-loader)
                  make-loader
                  nil))))))))

(defun prune-deps (deps)
  (prune-tree deps
              (lambda (x)
                (if (and (pkg-p (first x))
                         (not (file-p (first x)))
                         (find-package (pkg-name (first x))))
                    nil
                    t))
              (constantly t)))

(defun delete-package-tree (package-symbol)
  (mapc-tree
   (compute-package-dependencies/all/pkg package-symbol)
   (lambda (x)
     (when (pkg-filename x)
       (ignore-errors
         (clear-package (pkg-name x))
         (delete-package (pkg-name x)))))))

(defun load-package (package-symbol &optional tabula-rasa force)
  "Loads the package PACKAGE-SYMBOL along with its dependencies if it
  doesn't already exist.  If a package loader is found, it will be
  used to avoid computing dependencies.  If TABULA-RASA is t, then the
  package loader will be ignored and any package loaded will be
  cleared of all symbols, as if the package had just been created.  If
  FORCE is t, then the package will be loaded even if it already
  exists.  TABULA-RASA = t implies FORCE = t."
  (let* ((loaded-files)
         (g (lambda (x) ;; pkg
              (when (pkg-p x)
                (unless (find x loaded-files
                              :test #'pkg-equal) ;; Only load once
                  (let ((file (pkg-filename x)))
                    (when file
                      (load (fasl-file file))
                      (push x loaded-files)
                      (when (boundp '*faslpath-trace*)
                        (push (list :load (pkg-name x))
                              *faslpath-trace*)))))))))

    (cond (tabula-rasa
           (with-compilation-unit ()
              (let ((deps (compute-package-dependencies/all/pkg
                           package-symbol)))
                (do-tabula-rasa deps)
                (mapc g (squash-tree-for-load deps)))))

          (force
           (let ((loader (package-loader-name package-symbol *fasl-type*)))
             (if (and loader (probe-file loader))
                 (load loader)
                 (with-compilation-unit ()
                   (let ((deps (compute-package-dependencies/all/pkg
                                package-symbol)))
                     (mapc g (squash-tree-for-load deps)))))))

          ((not (find-package package-symbol))
           (let ((loader (package-loader-name package-symbol
                                              *fasl-type*)))
             (if (and loader (probe-file loader))
                 (load loader)
                 (with-compilation-unit ()
                   (let ((deps (compute-package-dependencies/all/pkg
                                package-symbol)))
                     (mapc g (squash-tree-for-load (prune-deps deps)))))))))
    package-symbol))


(defun compile-package (package-symbol &optional tabula-rasa make-loader)
  "Compiles and loads the package PACKAGE-SYMBOL along with its
  dependencies.  If a file doesn't need to be compiled, then it will
  only be loaded.

  If MAKE-LOADER is t then a package loader will be
  generated.  The name of the loader file is of the form
  \"foo-loader.lisp\", assuming PACKAGE-SYMBOL ends with \"FOO\".

  If TABULA-RASA is t then any package compiled or loaded will be cleared
  of all symbols, as if the package had just been created."
  (let* ((loaded-files)
         (loaded-filenames)
         (loaded-pkg)
         (*package* (find-package :cl-user))
         (deps (compute-package-dependencies/timestamps/all
                package-symbol)))
    (labels ((compile-subtree (tree &optional subtree-time)
               (cond ((null tree) nil)

                     ((pkg-p tree)
                      (with-slots (name
                                   filename
                                   source-timestamp
                                   object-timestamp) tree

                        (let ((p (find tree loaded-pkg
                                       :test #'pkg-equal)))
                          (if p
                              (pkg-source-timestamp p)
                              (let* ((f filename)
                                     (needed
                                      (when f
                                        (cond ((numberp subtree-time)
                                               (compile-needed?*
                                                (max source-timestamp subtree-time)
                                                object-timestamp))

                                              (t (compile-needed?*
                                                  source-timestamp
                                                  object-timestamp))))))

                                (if needed
                                    (progn
                                      (load (compile-file f))
                                      (when (boundp '*faslpath-trace*)
                                        (push `(:compile ,name) *faslpath-trace*)))
                                    (progn
                                      (when f
                                        (load (fasl-file f))
                                        (when (boundp '*faslpath-trace*)
                                          (push `(:load ,name) *faslpath-trace*)))))

                                (when f
                                  (push name loaded-files)
                                  (push tree loaded-pkg)
                                  (push f loaded-filenames))

                                source-timestamp)))))

                     ((listp tree)
                      (destructuring-bind (this-package &rest deps) tree
                        (let ((f (loop :for i :in deps
                                       :for x = (or (compile-subtree i) 0)
                                       :maximize x)))
                          (compile-subtree this-package f)))))))

      (when (check-package package-symbol)
        (with-compilation-unit ()
          (when tabula-rasa
            (do-tabula-rasa deps))
          (compile-subtree deps)
          (when make-loader
            (compile-file
             (write-package-loader/tree
              deps
              'load-always
              (if (stringp make-loader)
                  make-loader
                  nil)))))))))

