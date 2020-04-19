# Portable Condition System

## Description

This is an example implementation of the Common Lisp condition system, based on the original condition system implementation by Kent M. Pitman.

This code has been:
* forked from its [original source location](http://www.nhplace.com/kent/CL/Revision-18.lisp),
* cleaned to compile on contemporary Common Lisp implementations without warnings,
* downcased and adjusted to follow modern Lisp style standards,
* modernized to use ANSI CL functionalities, wherever applicable: for example, `defclass`, `defmethod`, `print-object`, `print-unreadable-object`, or `destructuring-bind`,
* updated to shadow the ANSI CL symbol list instead of the original CLtL1 symbol list and to follow the ANSI CL standard in functionality,
* documented via documentation strings for all suitable definition forms,
* made installable as a system-wide condition system that integrates with the host condition system,
* packaged as an ASDF system `portable-condition-system`,
* submitted to [Quicklisp](https://github.com/quicklisp/quicklisp-projects/issues/1833),
* tested via the ANSI-TEST test suites related to the condition system.

This system additionally defines a `common-lisp+portable-condition-system` package. It is equivalent to the `common-lisp` package, except all symbols related to the condition system are instead imported from the `portable-condition-system`, effectively overriding the condition system implementation from the host. This, along with running the `portable-condition-system.integration:install` function (see the [integration system manual](integration/README.md) for details), ensures that the host's condition system is overridden by the custom one defined by `portable-condition-system`.

## Background

The original comment from by Kent states:

> This is a sample implementation. It is not in any way intended as the definition of any aspect of the condition system. It is simply an existence proof that the condition system can be implemented.
>
> While this written to be "portable", this is not a portable condition system in that loading this file will not redefine your condition system. Loading this file will define a bunch of functions which work like a condition system. Redefining existing condition systems is beyond the goal of this implementation attempt.

This system has been created as a part of the efforts behind my upcoming book, *Common Lisp Condition System*. It is meant to be educational material for people who want to see how to implement a condition system from scratch. Additionally, it may be used for Common Lisp implementations which do not have a condition system themselves and would nonetheless like to adopt.

## Extending

The debugger itself is roughly extensible by means of defining new methods on the internal `run-debugger-command` generic function and pushing help-printing hooks onto `*help-hooks*`. See [`debugger.lisp`](debugger.lisp) for details.

## Tests

The test suite has been adapted from [ANSI-TEST](https://gitlab.common-lisp.net/ansi-test/ansi-test) at commit `c4679fdd9966`. To run it, evaluate `(asdf:test-system :portable-condition-system)` or manually load ASDF system `portable-condition-system/test` and run `(portable-condition-system/test:run)`.

## Example

Example debugger session:

```lisp
PORTABLE-CONDITION-SYSTEM> (tagbody :go
                              (let ((condition (make-condition 'type-error
                                                               :datum 42
                                                               :expected-type 'string)))
                                (restart-case (invoke-debugger condition)
                                  (abort () :report "Abort.")
                                  (retry () :report "Retry." (go :go))
                                  (fail () :report "Fail."))))
;; Debugger level 1 entered on TYPE-ERROR:
;; The value
;;   42
;; is not of type
;;   STRING.
;; Type :HELP for available commands.
[1] Debug> :help

;; This is the standard debugger of the Portable Condition System.
;; The debugger read-eval-print loop supports the standard REPL variables:
;;   *   **   ***   +   ++   +++   /   //   ///   -
;;
;; Available debugger commands:
;;  :HELP              Show this text.
;;  :EVAL <form>       Evaluate a form typed after the :EVAL command.
;;  :REPORT            Report the condition the debugger was invoked with.
;;  :CONDITION         Return the condition object the debugger was invoked with.
;;  :RESTARTS          Print available restarts.
;;  :RESTART <n>, <n>  Invoke a restart with the given number.
;;  :ABORT, :Q         Exit by calling #'ABORT.
;;
;; Any non-keyword non-integer form is evaluated.
[1] Debug> :restarts

;; Available restarts:
;;  0: [ABORT] Abort.
;;  1: [RETRY] Retry.
;;  2: [FAIL ] Fail.
[1] Debug> (error "recursive debugger time")

;; Debugger level 2 entered on SIMPLE-ERROR:
;; recursive debugger time
;; Type :HELP for available commands.
[2] Debug> :restarts

;; Available restarts:
;;  0: [ABORT] Return to debugger level 1.
;;  1: [ABORT] Abort.
;;  2: [RETRY] Retry.
;;  3: [FAIL ] Fail.
[2] Debug> 0

[1] Debug> (+ 2 2)

4
[1] Debug> :eval (+ 22 22)

44
[1] Debug> +

(+ 22 22)
[1] Debug> ***

4
[1] Debug> :q

NIL
PORTABLE-CONDITION-SYSTEM> 
```

## License

CC0 / Public Domain.

The test suite for this system is licensed under the [MIT license](t/LICENSE).
