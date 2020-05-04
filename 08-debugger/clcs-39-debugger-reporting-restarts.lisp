;;;; clcs-39-debugger-reporting-restarts.lisp

(defpackage #:clcs-39-debugger-reporting-restarts
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-39-debugger-reporting-restarts)

(defun debugger (condition hook)
  (declare (ignore hook))
  (let ((*print-escape* nil))
    (format t ";;~%;; Debugger entered on ~S:~%" (type-of condition))
    (format t ";; ~W~%" condition)
    (let ((restarts (compute-restarts condition)))
      (format t ";;~%;; Available restarts:~%")
      (dolist (restart restarts)
        (format t ";; [~W] ~W~%" (restart-name restart) restart))))
  (abort condition))

(defun test ()
  (let ((*debugger-hook* #'debugger)
        (condition (make-condition 'simple-error :format-control "We are in trouble.")))
    (invoke-debugger condition)))

#|

CL-USER> (clcs-39-debugger-reporting-restarts:test)
;;
;; Debugger entered on SIMPLE-ERROR:
;; We are in trouble.
;;
;; Available restarts:
;; [RETRY] Retry evaluating the form.
;; [ABORT] Return to the toplevel.
;;
;; ABORT restart invoked; returning to toplevel.

|#
