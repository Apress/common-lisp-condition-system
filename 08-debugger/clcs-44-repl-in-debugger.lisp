;;;; clcs-44-repl-in-debugger.lisp

(defpackage #:clcs-44-repl-in-debugger
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-44-repl-in-debugger)

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

(defun read-debug-expression (number-of-restarts)
  (format t ";; Enter a restart number to be invoked~%")
  (format t ";; or an expression to be evaluated.~%")
  (loop (format t "Debug> ")
        (let* ((form (read)))
          (if (and (integerp form) (< -1 form number-of-restarts))
              (return form)
              (print (eval form))))))

(defmacro with-abort-restart (&body body)
  (let ((level (gensym)))
    `(let ((,level *debugger-level*))
       (with-simple-restart (abort "Return to level ~D of the debugger." ,level)
         ,@body))))

(defun debugger (condition hook)
  (let ((*print-escape* nil)
        (*debugger-level* (1+ *debugger-level*)))
    (print-banner condition)
    (let ((restarts (compute-restarts condition)))
      (print-restarts restarts)
      (let* ((*debugger-hook* hook)
             (chosen-restart
              (with-abort-restart (read-debug-expression (length restarts)))))
        (when chosen-restart
          (with-abort-restart
            (invoke-restart-interactively (nth chosen-restart restarts)))))
      (let ((*debugger-level* (1- *debugger-level*)))
        (debugger condition hook)))))

(defvar *x* 24)

(defun test ()
  (trivial-custom-debugger:with-debugger (#'debugger)
    (assert (= *x* 42))))

#|

CL-USER> (clcs-44-repl-in-debugger:test)
;;
;; Debugger level 1 entered on SIMPLE-ERROR:
;; The assertion (= CLCS-44-REPL-IN-DEBUGGER::*X* 42) failed.
;;
;; Available restarts:
;; 0 [CONTINUE] Retry assertion.
;; 1 [RETRY] Retry evaluating the form.
;; 2 [ABORT] Return to the toplevel.
;;
;; Enter a restart number to be invoked
;; or an expression to be evaluated.

Debug> (setf clcs-44-repl-in-debugger::*x* 42)                 ; user input here
42

Debug> 0                                                       ; user input here
NIL

|#
