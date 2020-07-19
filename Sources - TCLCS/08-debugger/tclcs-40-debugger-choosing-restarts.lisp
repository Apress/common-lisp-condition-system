;;;; clcs-40-debugger-choosing-restarts.lisp

(defpackage #:clcs-40-debugger-choosing-restarts
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-40-debugger-choosing-restarts)

(defun read-valid-restart-number (number-of-restarts)
  (loop (format t ";;~%;; Invoke restart number: ")
        (let* ((line (read-line *query-io*))
               (integer (parse-integer line :junk-allowed t)))
          (when (and integer (< -1 integer number-of-restarts))
            (return integer)))))

(defun debugger (condition hook)
  (let ((*print-escape* nil))
    (format t ";;~%;; Debugger entered on ~S:~%" (type-of condition))
    (format t ";; ~W~%" condition)
    (let ((restarts (compute-restarts condition)))
      (format t ";;~%;; Available restarts:~%")
      (loop for i from 0
            for restart in restarts do
              (format t ";; ~D [~W] ~W~%" i (restart-name restart) restart))
      (let ((chosen-restart (read-valid-restart-number (length restarts)))
            (*debugger-hook* hook))
        (invoke-restart-interactively (nth chosen-restart restarts)))
      (debugger condition hook))))

(defun test ()
  (let ((*debugger-hook* #'debugger)
        (condition (make-condition 'simple-error :format-control "We are in trouble.")))
    (invoke-debugger condition)))

#|

CL-USER> (clcs-40-debugger-choosing-restarts:test)
;;
;; Debugger entered on SIMPLE-ERROR:
;; We are in trouble.
;;
;; Available restarts:
;; 0 [RETRY] Retry evaluating the form.
;; 1 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 0                                    ; user input here
;;
;; Debugger entered on SIMPLE-ERROR:
;; We are in trouble.
;;
;; Available restarts:
;; 0 [RETRY] Retry evaluating the form.
;; 1 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 3                                    ; user input here
;;
;; Invoke restart number: forty-two                            ; user input here
;;
;; Invoke restart number:                                      ; empty line provided
;;
;; Invoke restart number: 1                                    ; user input here
;;
;; ABORT restart invoked; returning to toplevel.

|#
