# The Common Lisp Condition System - companion code

This is a repository containing companion code for the book *The Common Lisp Condition System*.

The code is structured in folders, one folder for each topic mentioned in the book. Each Lisp file in each folder is a single-package self-contained snapshot showing the state of the code we create in our book at each stage of its writing. The symbols exported from each package are test functions that can be executed to demonstrate the behaviour of code at that particular moment in the book; example output of each test function is placed in the comments of each source file.

This system depends on [`trivial-custom-debugger`](https://github.com/phoe/trivial-custom-debugger) for installing a custom debugger function in place of the system-provided one.

`(ql:quickload :tclcs-code)` will load this system from Quicklisp.

The code for the second part of the book has been adapted from the [Portable Condition System](https://github.com/phoe/portable-condition-system) and is therefore not included here. Please see the CLCS repository for an updated and extended version of the code included in the book.

The original code for implementations of dynamic variables in C has been contributed by [Marco Heisig](https://github.com/marcoheisig), [Michael Raskin](https://gitlab.common-lisp.net/mraskin), and [Gilbert Baumann](http://clim.rocks/).
