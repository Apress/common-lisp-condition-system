;;;; clcs-38-debugger-reporting-condition-type.lisp

(defpackage #:clcs-38-debugger-reporting-condition-type
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-38-debugger-reporting-condition-type)

(defun debugger (condition hook)
  (declare (ignore hook))
  (let ((*print-escape* nil))
    (format t ";;~%;; Debugger entered on ~S:~%" (type-of condition))
    (format t ";; ~W~%" condition))
  (abort condition))

(defun test ()
  (let ((*debugger-hook* #'debugger)
        (condition (make-condition 'simple-error :format-control "We are in trouble.")))
    (invoke-debugger condition)))

#|

CL-USER> (clcs-38-debugger-reporting-condition-type:test)
;;
;; Debugger entered on SIMPLE-ERROR:
;; We are in trouble.
;;
;; ABORT restart invoked; returning to toplevel.

|#
