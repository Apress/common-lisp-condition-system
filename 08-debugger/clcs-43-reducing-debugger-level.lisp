;;;; clcs-43-reducing-debugger-level.lisp

(defpackage #:clcs-43-reducing-debugger-level
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-43-reducing-debugger-level)

(defvar *debugger-level* 0)

(defun print-banner (condition)
  (format t ";;~%;; Debugger level ~D entered on ~S:~%"
          *debugger-level* (type-of condition))
  (format t ";; ~W~%" condition))

(defun print-restarts (restarts)
  (format t ";;~%;; Available restarts:~%")
  (loop for i from 0
        for restart in restarts do
          (format t ";; ~D [~W] ~W~%" i (restart-name restart) restart)))

(defun read-valid-restart-number (number-of-restarts)
  (loop (format t ";;~%;; Invoke restart number: ")
        (let* ((line (read-line *query-io*))
               (integer (parse-integer line :junk-allowed t)))
          (when (and integer (< -1 integer number-of-restarts))
            (return integer)))))

(defun debugger (condition hook)
  (let ((*print-escape* nil)
        (*debugger-level* (1+ *debugger-level*)))
    (print-banner condition)
    (let ((restarts (compute-restarts condition)))
      (print-restarts restarts)
      (let ((chosen-restart (read-valid-restart-number (length restarts)))
            (*debugger-hook* hook)
            (current-debugger-level *debugger-level*))
        (with-simple-restart
            (abort "Return to level ~D of the debugger." current-debugger-level)
          (invoke-restart-interactively (nth chosen-restart restarts))))
      (debugger condition hook))))

(defun test ()
  (trivial-custom-debugger:with-debugger (#'debugger)
    (let ((x 42))
      (check-type x string)
      x)))

#|

CL-USER> (clcs-43-reducing-debugger-level:test)
;;
;; Debugger level 1 entered on SIMPLE-TYPE-ERROR:
;; The value of X is 42, which is not of type STRING.
;;
;; Available restarts:
;; 0 [STORE-VALUE] Supply a new value for X.
;; 1 [RETRY] Retry evaluating the form.
;; 2 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 0                                    ; user input here
;;
;; Enter a form to be evaluated:                               ; user input here

(let ((maybe-result "some string"))
  (cerror "Continue." "About to supply ~S." maybe-result)
  maybe-result)

;;
;; Debugger level 2 entered on SIMPLE-ERROR:
;; About to supply "some string".
;;
;; Available restarts:
;; 0 [CONTINUE] Continue.
;; 1 [ABORT] Return to level 1 of the debugger.
;; 2 [RETRY] Retry evaluating the form.
;; 3 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 1                                    ; user input here
;;
;; Debugger level 1 entered on SIMPLE-TYPE-ERROR:
;; The value of X is 42, which is not of type STRING.
;;
;; Available restarts:
;; 0 [STORE-VALUE] Supply a new value for X.
;; 1 [RETRY] Retry evaluating the form.
;; 2 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 2                                    ; user input here
;;
;; ABORT restart invoked; returning to toplevel.
|#
