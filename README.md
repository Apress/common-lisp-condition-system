# Portable Condition System

## Warning

**THIS SYSTEM IS A WORK IN PROGRESS. THE BELOW README IS NOT YET FULLY TRUE.**

## Description

This is an example implementation of the Common Lisp condition system, based on the original condition system implementation by Kent M. Pitman.

This code ~~has been~~ will be:
* forked from its [original source location](http://www.nhplace.com/kent/CL/Revision-18.lisp),
* cleaned to compile on contemporary Common Lisp implementations without warnings,
* downcased and adjusted to follow modern Lisp style standards,
* modernized to use ANSI CL functionalities, wherever applicable: for example, `print-unreadable-object`, `defmethod print-object`, or `destructuring-bind`,
* updated to shadow the ANSI CL symbol list instead of the original CLtL1 symbol list and to follow the ANSI CL standard in functionality,
* packaged as an ASDF system `portable-condition-system`,
* made installable as a system-wide condition system that integrates with the host condition system.

TODO actually implement all of the above, lol.

This system additionally defines a `common-lisp+portable-condition-system` package. It is equivalent to the `common-lisp` package, except all symbols related to the condition system are instead imported from the `portable-condition-system`, effectively overriding the condition system implementation from the host. This, along with running the `portable-condition-system/integration:install` function (see [Integration](#integration)), ensures that the host's condition system is overrided by the custom one defined by `portable-condition-system`.

## Background

The original comment from by Kent states:

```lisp
;;; This is a sample implementation. It is not in any way intended as the definition
;;; of any aspect of the condition system. It is simply an existence proof that the
;;; condition system can be implemented.
;;;
;;; While this written to be "portable", this is not a portable condition system
;;; in that loading this file will not redefine your condition system. Loading this
;;; file will define a bunch of functions which work like a condition system. Redefining
;;; existing condition systems is beyond the goal of this implementation attempt.
```

## Integration

The task of integrating this condition system with the host Lisp's one is handled by the ASDF system `portable-condition-system/integration` which loads a package with the same name. Executing the `install` function from that package installs a system-wide debugger defined in `portable-condition-system`. It additionally installs a translation layer which translates condition objects and restarts from the host's condition system into condition objects and restarts used by this system's debugger for better system integration of the `portable-condition-system` debugger.

## License

CC0 / Public Domain.
