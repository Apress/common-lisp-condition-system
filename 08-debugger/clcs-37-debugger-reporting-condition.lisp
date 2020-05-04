;;;; clcs-37-debugger-reporting-condition.lisp

(defpackage #:clcs-37-debugger-reporting-condition
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-37-debugger-reporting-condition)

(defun debugger (condition hook)
  (declare (ignore hook))
  (let ((*print-escape* nil))
    (format t ";; Aborting: ~W~%" condition))
  (abort condition))

(defun test ()
  (let ((*debugger-hook* #'debugger)
        (condition (make-condition 'simple-error :format-control "We are in trouble.")))
    (invoke-debugger condition)))

#|

CL-USER> (clcs-37-debugger-reporting-condition:test)
;; Aborting: We are in trouble.
;;
;; ABORT restart invoked; returning to toplevel.

|#
