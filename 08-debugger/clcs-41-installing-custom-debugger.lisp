;;;; clcs-41-installing-custom-debugger.lisp

(defpackage #:clcs-41-installing-custom-debugger
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-41-installing-custom-debugger)

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

(defun test-1 ()
  (trivial-custom-debugger:with-debugger (#'debugger)
    (break "Breaking with ~D." 42)))

(defun test-2 ()
  (trivial-custom-debugger:with-debugger (#'debugger)
    (let ((*break-on-signals* 'error))
      (handler-case (signal 'simple-error :format-control "Error: ~D."
                                          :format-arguments '(42))
        (error (condition)
          (format t ";; Handling a ~W.~%" (type-of condition)))))))

#|

CL-USER> (clcs-41-installing-custom-debugger:test-1)
;;
;; Debugger entered on SIMPLE-CONDITION:
;; Breaking with 42.
;;
;; Available restarts:
;; 0 [CONTINUE] Return from BREAK.
;; 1 [RETRY] Retry evaluating the form.
;; 2 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 0                                    ; user input here
NIL

CL-USER> (clcs-41-installing-custom-debugger:test-2)
;;
;; Debugger entered on SIMPLE-CONDITION:
;; Error: 42.
;; (BREAK was entered because of *BREAK-ON-SIGNALS*.)
;;
;; Available restarts:
;; 0 [CONTINUE] Return from BREAK.
;; 1 [RESET] Set *BREAK-ON-SIGNALS* to NIL and continue.
;; 2 [REASSIGN] Return from BREAK and assign a new value to *BREAK-ON-SIGNALS*.
;; 3 [RETRY] Retry evaluating the form.
;; 4 [ABORT] Return to the toplevel.
;;
;; Invoke restart number: 0                                    ; user input here
;; Handling a SIMPLE-ERROR.
NIL

|#
