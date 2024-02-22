                               FASLPATH
                               ========

Author: Peter von Etter <peterve@gmail.com>
Date: 2014-07-14 20:05:46 EEST


An introduction to the Faslpath loader.

Table of Contents
=================
1 Introduction
2 Obtaining
3 Installation
4 Usage
5 More details
6 Extra features
7 Package Interface
    7.1 *FASLPATH*
    7.2 COMPILE-PACKAGE
    7.3 LOAD-PACKAGE
    7.4 COMPILE-PACKAGE-FROM-SCRATCH
8 Shell scripts
    8.1 compile-lisp-package
    8.2 make-lisp-core
    8.3 invoke-lisp-package
    8.4 invoke-lisp-core
    8.5 start-lisp-package
    8.6 start-lisp-core
9 Supported implementations
    9.1 Porting to other implementations


1 Introduction
===============

Faslpath is a build tool for Common Lisp that is intended to be very
easy to use, even for beginner lisp programmers.  It does not require
separate build files.  Instead, it relies on naming conventions to
statically resolve dependencies between files.  An illustrative
example is probably the easiest way to explain how it works.

Suppose we have the following directory structure:

- lib
  - foo
    - foo.lisp
    - util.lisp
  - bar
     - bar.lisp
     - macros.lisp
     - util.lisp

and that the files look like this

foo/foo.lisp:

 (defpackage :foo.foo
   (:use :cl
         :foo.util
         :bar.bar)
   (:export #:FOO))
 ...

foo/util.lisp:

 (defpackage :foo.util
   (:use :cl)
   (:export #:FOO-TOOL))
 ...

bar/bar.lisp:

 (defpackage :bar.bar
   (:use :cl
         :bar.macros
         :bar.util)
   (:export #:BAR #:WITH-BAR))
 ...

bar/util.lisp:

 (defpackage :bar.util
   (:use :cl
         :bar.macros)
   (:export #:BAR-TOOL))
 ...

bar/macros.lisp:

 (defpackage :bar.macros
   (:use :cl)
   (:export #:WITH-BAR))
 ...

Now, given these files, we can call

 (faslpath:compile-package :foo.foo)

This will tell the faslpath loader to compile the package :foo.foo.
It will automatically figure out the dependencies of each file and
compile them in the right order.  It does so by examining the
defpackage form in each file and assuming that a package
named :bar.util will be found in the file bar/util.lisp.  The only
configuration that's required is to tell the faslpath loader about the
"lib" directory by pushing #P/path/to/lib/ onto the
faslpath:*faslpath* variable.


2 Obtaining
============

  Faslpath is available at [http://code.google.com/p/faslpath/]

3 Installation
===============

1. Run the script "compile-IMPL.sh" where IMPL is one of the supported
   implementations.  It will compile faslpath and run some tests.  If
   you see a stack trace, it probably means that one of the tests
   failed, and that the program should not be used.

2. Add the following to your implementation's init file, replacing the
   paths appropriately:

 (load "/home/john/lisp/faslpath/faslpath.fasl")
 (use-package :faslpath)
 (setf *faslpath* '(#P"/home/john/lisp/"))

3. Copy any scripts you want from the "scripts" directory into a
   directory such as "~/bin".  They will give you easy access to your
   packages.

After these steps, code placed in /home/john/lisp/ and its
subdirectories should be visible to the faslpath loader.


4 Usage
========

To make use of faslpath, simply name your source files according to
their location and make sure that the first form in each file is a
defpackage form.  Dependencies are specified with the "USE" list in
the defpackage form.

Example:

In the file my-app/main.lisp you need:

 (defpackage :my-app.main
   (:use :cl
         :my-app.tools
         :my-app.macros
         :my-app.asdf-deps
         :my-external-lib.main)
   (:export #:RUN-MY-APP
            #:MAIN))

 (in-package :my-app.main)

 [ ... Code follows ... ]

Now COMPILE-PACKAGE and LOAD-PACKAGE will know what the symbol
"MY-APP.MAIN" means, and will be able to find it.  One could now, for
instance, run the shell command

$ invoke-lisp-package my-app.main

This would load the MY-APP.MAIN package and its dependencies and then
call the MAIN function in that package.


5 More details
===============

Dependencies are extracted directly from the source files by mapping
package names to source files.  This enables us to "overload" the use
list in defpackage to indicate file dependencies in addition to normal
package dependencies.  Currently, the mapping from packages to source
files used is the familiar "dotted" notation, i.e. a package named
"FOO.BAR.QWE" represents the relative pathname "foo/bar/qwe.lisp".

The faslpath loader determines the dependencies of a lisp file by
probing the file for a defpackage form and resolving the packages in
the use list according to the package->file mapping used.  The probing
is done by READing the first form in the file.  As a result of this,
the file does not get loaded.

In order for all of this to work we need to adopt a couple of
conventions:

- The name of a package should match the location of the source file.
  In other words, the file "foo/bar/qwe.lisp" should contain a
  defpackage form that defines a package named "FOO.BAR.QWE".

- All dependencies should be specified in the defpackage use list.

- The defpackage form should be the *first* form in the file.
  Alternatively, a quoted list such as '(:foo.util :bar.bar) can be
  used as the first form to specify dependencies, but this isn't
  encouraged.

The faslpath:*faslpath* variable should contain a list of directory pathnames
that should be searched when resolving dependencies.  It is probably
best to do set this this in the lisp init file, like this, for
example:

 (load "/path/to/faslpath.fasl")
 (use-package :faslpath)
 (setf *faslpath* '(#P/path/to/my/libs/
                    #P/path/to/some/other/libs/))


6 Extra features
=================

If you absolutely do not want to have a defpackage in each file, or if
you need finer control over which symbols are imported into in a
package, then you can specify dependencies with a quoted list as the
first form in a file.  This is a somewhat ugly hack, and may change in
the future.

Symbols in the quoted list will be interpreted as package
dependencies.  They will be searched for in faslpath:*faslpath* and
loaded if necessary.  Note that a package is considered "loaded" if
(find-package ...) returns non-nil.

Strings in the quoted list will be interpreted as files, relative to
the current file, which will need to be loaded (or compiled) before
the current file is loaded.  The files are assumed to belong to the
same "unit" as the current file and will be loaded unconditionally if
the current file ever becomes loaded.


Example:

 '(:my-app.tools
   :my-app.macros
   :my-app.asdf-deps
   :my-external-lib.main
   "file-1"
   "file-2"
   "subdir/file-3"
   "subdir/file-4")

 (in-package :my-app.main)

 [ ... Code follows ... ]

7 Package Interface
====================

The following symbols are exported by the FASLPATH package:

7.1 *FASLPATH*
---------------

   (special variable)

   A list of directory pathnames that indicates where the root package
   directories are located.


7.2 COMPILE-PACKAGE
--------------------

   (function)

:COMPILE-PACKAGE PACKAGE-SYMBOL &OPTIONAL TABULA-RASA MAKE-LOADER

   Compiles and loads the package PACKAGE-SYMBOL along with its
   dependencies.  If a file doesn't need to be compiled, then it will
   only be loaded.

   If MAKE-LOADER is t then a package loader will be
   generated.  The name of the loader file is of the form
   "foo-loader.lisp", assuming PACKAGE-SYMBOL ends with "FOO".

   If TABULA-RASA is t then any package compiled or loaded will be cleared
   of all symbols, as if the package had just been created.

   Example:
    (compile-package :my-app.main)

   This will make faslpath look for the file "my-app/main.lisp" in the
   directories specified by faslpath:*faslpath* and compile it and its
   dependencies.

7.3 LOAD-PACKAGE
-----------------

   (function)

   :LOAD-PACKAGE PACKAGE-SYMBOL &OPTIONAL TABULA-RASA FORCE

   Loads the package PACKAGE-SYMBOL along with its dependencies if it
   doesn't already exist.  If a package loader is found, it will be
   used to avoid computing dependencies.  If TABULA-RASA is t, then the
   package loader will be ignored and any package loaded will be
   cleared of all symbols, as if the package had just been created.  If
   FORCE is t, then the package will be loaded even if it already
   exists.  TABULA-RASA = t implies FORCE = t.

   Example:
    (load-package :my-app.main)

   This will make faslpath look for the file "my-app/main-loader.fasl"
   in the directories specified by faslpath:*faslpath* and load it.  If
   the file isn't found, then a dependency tree for the file
   "my-app/main.lisp" will be constructed and used to load the required
   fasl files.  No files will be recompiled as a result of calling
   LOAD-PACKAGE.


7.4 COMPILE-PACKAGE-FROM-SCRATCH
---------------------------------

   (function)

   :COMPILE-PACKAGE-FROM-SCRATCH PACKAGE-SYMBOL &OPTIONAL TABULA-RASA

   Unconditionally compiles and loads PACKAGE-SYMBOL and all of its
   dependencies.

8 Shell scripts
================

A number of shell scripts are provided for convenience

8.1 compile-lisp-package
-------------------------
:compile-lisp-package PACKAGE-NAME

    Compiles the package named PACKAGE-NAME, then exits.

    Example:
    $ compile-lisp-package my-app.main

8.2 make-lisp-core
-------------------
:make-lisp-core PACKAGE-NAME

    Loads the package named PACKAGE-NAME and saves the running lisp
    image.

    Note: This script requires that faslpath can find its own
          directory.

8.3 invoke-lisp-package
------------------------
:invoke-lisp-package PACKAGE-NAME

    Loads the package named PACKAGE-NAME and calls the function in that
    package specified by the symbol MAIN, then exits.


8.4 invoke-lisp-core
---------------------
:invoke-lisp-core PACKAGE-NAME

    Starts the core named PACKAGE-NAME and calls the function in that
    package specified by the symbol MAIN, then exits.


8.5 start-lisp-package
-----------------------
:start-lisp-package PACKAGE-NAME

    Starts a lisp and loads the package named PACKAGE-NAME.


8.6 start-lisp-core
--------------------
:start-lisp-core PACKAGE-NAME

    Starts the core named PACKAGE-NAME.

These scripts will give easy access to the lisp packages assuming
faslpath is loaded by the lisp init file (or otherwise present in the
running core).


9 Supported implementations
============================

Currently the following implementations have been tested:

- SBCL 1.0.9, 1.0.19 and 1.0.22
- CLISP 2.42
- CCL 1.3 (tested on OS X 10.5.6)

9.1 Porting to other implementations
-------------------------------------

Faslpath should be easily portable to other implementations.

In order to port it to another implementation the following is
currently needed:

1. Fill in the blanks in impl.lisp and at the beginning
   of loader.lisp
2. Port the shell scripts in the scripts directory
3. Create a compile-IMPL.sh script

-----
