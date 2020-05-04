# Common Lisp Condition System - companion code

This is a repository containing companion code for the first part of the upcoming book, *Common Lisp Condition System*. This repository is still a work-in-progress, as is the book.

The code is structured in folders, one folder for each topic mentioned in the book. Each Lisp file in each folder is a single-package self-contained snapshot showing the state of the code we create in our book at each stage of its writing. The symbols exported from each package are test functions that can be executed to demonstrate the behaviour of code at that particular moment in the book; example output of each test function is placed in the comments of each source file.

`(ql:quickload :clcs-code)` will load the system from Quicklisp.

The code for the second part of the book has been adapted from the [Portable Condition System](https://github.com/phoe/portable-condition-system) and is therefore not included here. Please see the CLCS repository for an updated and extended version of the code included in the book.
