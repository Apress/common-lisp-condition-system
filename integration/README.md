# Portable Condition System Integration

## Overview

The task of integrating PCS with the host Lisp's one is handled by the ASDF system `portable-condition-system.integration` which loads a package with the same name.

* Executing the `install` function from that package installs a system-wide debugger defined in PCS.
* Executing `trivial-custom-debugger:with-debugger` with the `portable-condition-system.integration:debugger` function changes the debugger within dynamic scope only.

This system additionally contains a translation layer which translates condition objects and restarts from the host's condition system into condition objects and restarts used by this system's debugger for better integration of the host condition system within the PCS debugger.

## Details

The PCS condition type hierarchy and PCS handler stack are completely independent of the host's, meaning that the host `condition` class and PCS `condition` class are distinct and not a subclass of one another. The host's `signal` will not trigger PCS handlers and vice versa.

The host's restart objects are invisible to PCS `compute-restarts`/`find-restart` unless the wrapper macro `with-host-restarts` is used. This macro computes the host restarts using the optionally provided host condition object and adds the host restarts to the bottom of the handler stack, which means that PCS restarts will always appear above host restarts. In addition, host restarts are reported with a `(*) ` prepended to their report.

The debugger commands `:abort`, `:q`, `:continue`, and `:c` properly recognize the host's `abort` and `continue` restarts.

The PCS debugger is allowed to be entered with a host condition object. In such a case, the condition is first translated to a PCS condition by calling `host-condition-to-pcs` on it. Afterwards, the PCS condition is signaled, and the debugger is entered normally with the PCS condition object.

## Limitations

The main limitation of the integration system is that when the debugger is entered with a host condition, the resulting PCS condition is always signaled and therefore allows the PCS handler stack to handle the condition.

The reason for that is that the debugger has no means of knowing if it was called directly (via `invoke-debugger` or `break`) or from a signaling function (via `error` or `cerror`). Therefore, we are required to define the behaviour when the debugger is entered with a host condition in terms of never signaling the wrapped PCS condition, or always signaling it. The former case is unacceptable, since the host's `error` function will then make it impossible to define a handler that catches all errors, host or foreign ones, and therefore break, among others, PCS `ignore-errors` and `handler-case`. Therefore, we choose the latter case, and always signal the translated PCS condition when entering the debugger.

This has an unfortunate site effect of interfering with the host's `invoke-debugger` and `break` functions, since it becomes possible for the debugger entry to be handled by the PCS handler stack. This will be the case if the host condition object that the debugger was entered with becomes translated to a `foreign-condition` for which a PCS handler is defined. For a solution to this problem, please use the PCS-provided `invoke-debugger` and `break` functions instead of the host's original ones.

## Extending

It is possible to extend the means in which the integration system wraps host conditions in order to be able to use the newly defined condition types in e.g. `handler-case`.

This extension is possble by:
* defining condition types that are subtype of `foreign-condition`, `foreign-warning`, or `foreign-error`,
* defining methods on the `host-condition-to-pcs` generic function.

For example, if we would like to wrap `cl:type-error` into a new PCS condition type named `foreign-type-error`, we can perform it in the following way:

```lisp
(define-condition foreign-type-error (foreign-condition type-error) ())

(defmethod host-condition-to-pcs ((condition cl:type-error))
  (make-condition 'foreign-type-error
                  :wrapped-condition condition
                  :datum (cl:type-error-datum condition)
                  :expected-type (cl:type-error-expected-type condition)))
```

## Tests

TODO

## Example

Example integrated debugger session:

```lisp
INTEGRATION> (with-debugger (#'debugger)
               (+ 2 :two))
;; Debugger level 1 entered on FOREIGN-ERROR:
;; Foreign condition FOREIGN-ERROR was signaled:
;; Value of :TWO in (+ 2 :TWO) is :TWO, not a NUMBER.
;; Type :HELP for available commands.
[1] Debug> :abort
; Evaluation aborted on #<COMMON-LISP:SIMPLE-TYPE-ERROR expected-type: NUMBER datum: :TWO>.

INTEGRATION> (with-debugger (#'debugger)
               (cl:break "This is hopelessly broken."))

;; Debugger level 1 entered on FOREIGN-CONDITION:
;; Foreign condition FOREIGN-CONDITION was signaled:
;; This is hopelessly broken.
;; Type :HELP for available commands.
[1] Debug> :restarts

;; Available restarts:
;;  0: [CONTINUE] (*) Return from COMMON-LISP:BREAK.
;;  1: [RETRY   ] (*) Retry SLIME REPL evaluation request.
;;  2: [ABORT   ] (*) Return to SLIME's top level.
;;  3: [ABORT   ] (*) abort thread (#<THREAD "repl-thread" RUNNING {100CBF9DE3}>)
[1] Debug> 2
; Evaluation aborted on NIL.

INTEGRATION>
```

## License

CC0 / Public Domain.
