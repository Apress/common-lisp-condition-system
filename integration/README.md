# Portable Condition System Integration

## Overview

The task of integrating PCS with the host Lisp's one is handled by the ASDF system `portable-condition-system.integration` which loads a package with the same name.

Executing the `install` function from that package installs a system-wide debugger defined in PCS. Executing `trivial-custom-debugger:with-debugger` with the `portable-condition-system.integration:debugger` function changes the debugger within dynamic scope only.

This system additionally contains a translation layer which translates condition objects and restarts from the host's condition system into condition objects and restarts used by this system's debugger for better integration of the host condition system within the PCS debugger.

## Details

TODO

## Extending

TODO

## Tests

TODO

## Example

Example integrated debugger session:

```lisp
PORTABLE-CONDITION-SYSTEM.INTEGRATION> (with-debugger (#'debugger)
                                         (+ 2 :two))
;; Debugger level 1 entered on FOREIGN-ERROR:
;; Foreign condition FOREIGN-ERROR was signaled:
;; Value of :TWO in (+ 2 :TWO) is :TWO, not a NUMBER.
;; Type :HELP for available commands.
[1] Debug> :abort
; Evaluation aborted on #<COMMON-LISP:SIMPLE-TYPE-ERROR expected-type: NUMBER datum: :TWO>.

PORTABLE-CONDITION-SYSTEM.INTEGRATION> (with-debugger (#'debugger)
                                         (cl:break "This is hopelessly broken."))
;; Debugger level 1 entered on FOREIGN-CONDITION:
;; Foreign condition FOREIGN-CONDITION was signaled:
;; This is hopelessly broken.
;; Type :HELP for available commands.
[1] Debug> :restarts

;; Available restarts:
;;  0: [CONTINUE] Return from COMMON-LISP:BREAK.
;;  1: [RETRY   ] Retry SLIME REPL evaluation request.
;;  2: [ABORT   ] Return to SLIME's top level.
;;  3: [ABORT   ] abort thread (#<THREAD "repl-thread" RUNNING {100CBF9DE3}>)
[1] Debug> 2
; Evaluation aborted on NIL.
```

## License

CC0 / Public Domain.
