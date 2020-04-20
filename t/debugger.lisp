;;;; t/debugger.lisp

(in-package #:portable-condition-system/test)

(defun run-debugger-command
    (command input-string &optional condition &rest args)
  (with-input-from-string (input input-string)
    (with-output-to-string (output)
      (let ((stream (make-two-way-stream input output)))
        (apply #'portable-condition-system::run-debugger-command
               command stream condition args)))))

(deftest debugger.eval.1
  (run-debugger-command :eval "(+ 2 2)")
  "4")

(deftest debugger.eval.2
  (handler-case (run-debugger-command :eval "(error \"42\")")
    (error () 'good))
  good)

(deftest debugger.report.1
  (let ((*package* (find-package :keyword))
        (condition (make-condition 'condition)))
    (run-debugger-command :report "" condition))
  ";; Debugger level 0 entered on PORTABLE-CONDITION-SYSTEM:CONDITION:
;; Condition PORTABLE-CONDITION-SYSTEM:CONDITION was signaled.
")

(deftest debugger.report.2
  (let ((*package* (find-package :keyword))
        (portable-condition-system::*debug-level* 42)
        (condition (make-condition 'type-error :datum 42
                                               :expected-type :keyword)))
    (run-debugger-command :report "" condition))
  ";; Debugger level 42 entered on PORTABLE-CONDITION-SYSTEM:TYPE-ERROR:
;; The value
;;   42
;; is not of type
;;   :KEYWORD.
")

(deftest debugger.report.3
  (let* ((*package* (find-package :keyword))
         (error-fn (lambda (&rest args)
                     (declare (ignore args))
                     (error "Error reporting condition")))
         (condition (make-condition 'simple-condition
                                    :format-control error-fn)))
    (run-debugger-command :report "" condition))
  ";; Debugger level 0 entered on PORTABLE-CONDITION-SYSTEM:SIMPLE-CONDITION:
;; #<error while reporting condition>
")

(defvar *debugger.condition* (make-condition 'condition))

(deftest debugger.condition.1
  (eqt (first (portable-condition-system::run-debugger-command
               :condition nil *debugger.condition*))
       *debugger.condition*)
  t)

(deftest debugger.abort.1
  (run-debugger-command :abort "" (make-condition 'condition))
  ";; There is no active ABORT restart.
")

(deftest debugger.abort.2
  (restart-case (run-debugger-command :abort "" (make-condition 'condition))
    (abort () 'good))
  good)

(deftest debugger.q.1
  (run-debugger-command :q "" (make-condition 'condition))
  ";; There is no active ABORT restart.
")

(deftest debugger.q.2
  (restart-case (run-debugger-command :q "" (make-condition 'condition))
    (abort () 'good))
  good)

(deftest debugger.continue.1
  (run-debugger-command :continue "" (make-condition 'condition))
  ";; There is no active CONTINUE restart.
")

(deftest debugger.continue.2
  (restart-case (run-debugger-command :continue "" (make-condition 'condition))
    (continue () 'good))
  good)

(deftest debugger.c.1
  (run-debugger-command :c "" (make-condition 'condition))
  ";; There is no active CONTINUE restart.
")

(deftest debugger.c.2
  (restart-case (run-debugger-command :c "" (make-condition 'condition))
    (continue () 'good))
  good)

(deftest debugger.restarts.1
  (run-debugger-command :restarts "" (make-condition 'condition))
  ";; No available restarts.
")

(deftest debugger.restarts.2
  (restart-case (run-debugger-command :restarts "" (make-condition 'condition))
    (abort () :report "Abort.")
    (retry () :report "Retry.")
    (fail () :report "Fail."))
  ";; Available restarts:
;;  0: [ABORT] Abort.
;;  1: [RETRY] Retry.
;;  2: [FAIL ] Fail.
")

(deftest debugger.restarts.3
  (restart-case (run-debugger-command :restarts "" (make-condition 'condition))
    (abort () :report (lambda (stream)
                        (declare (ignore stream))
                        (error "Error reporting restart"))))
  ";; Available restarts:
;;  0: [ABORT] #<error while reporting restart>
")

(deftest debugger.restart.1
  (run-debugger-command :restart "" (make-condition 'condition) 0)
  ";; There is no restart with number 0.
")

(deftest debugger.restart.2
  (restart-case (run-debugger-command :restart "" (make-condition 'condition) 1)
    (abort () 'bad)
    (retry () 'good)
    (fail () 'ugly))
  good)

(deftest debugger.help.1
  (run-debugger-command :help "")
  ";; This is the standard debugger of the Portable Condition System.
;; The debugger read-eval-print loop supports the standard REPL variables:
;;   *   **   ***   +   ++   +++   /   //   ///   -
;;
;; Available debugger commands:
;;  :HELP              Show this text.
;;  :EVAL <form>       Evaluate a form typed after the :EVAL command.
;;  :REPORT            Report the condition the debugger was invoked with.
;;  :CONDITION         Return the condition the debugger was invoked with.
;;  :RESTARTS          Print available restarts.
;;  :RESTART <n>, <n>  Invoke a restart with the given number.
;;
;; Any non-keyword non-integer form is evaluated.
")

(deftest debugger.help.2
  (restart-case (run-debugger-command :help "")
    (abort ())
    (continue ()))
  ";; This is the standard debugger of the Portable Condition System.
;; The debugger read-eval-print loop supports the standard REPL variables:
;;   *   **   ***   +   ++   +++   /   //   ///   -
;;
;; Available debugger commands:
;;  :HELP              Show this text.
;;  :EVAL <form>       Evaluate a form typed after the :EVAL command.
;;  :REPORT            Report the condition the debugger was invoked with.
;;  :CONDITION         Return the condition the debugger was invoked with.
;;  :RESTARTS          Print available restarts.
;;  :RESTART <n>, <n>  Invoke a restart with the given number.
;;  :ABORT, :Q         Invoke an ABORT restart.
;;  :CONTINUE, :C      Invoke a CONTINUE restart.
;;
;; Any non-keyword non-integer form is evaluated.
")

(deftest debugger.help.3
  (let* ((condition (make-condition 'simple-condition
                                    :format-control "Bar"))
         (hook (lambda (condition stream)
                 (format stream "~&;;  :FOO               ~A~%" condition)))
         (portable-condition-system::*help-hooks*
           (list hook)))
    (run-debugger-command :help "" condition))
  ";; This is the standard debugger of the Portable Condition System.
;; The debugger read-eval-print loop supports the standard REPL variables:
;;   *   **   ***   +   ++   +++   /   //   ///   -
;;
;; Available debugger commands:
;;  :HELP              Show this text.
;;  :EVAL <form>       Evaluate a form typed after the :EVAL command.
;;  :REPORT            Report the condition the debugger was invoked with.
;;  :CONDITION         Return the condition the debugger was invoked with.
;;  :RESTARTS          Print available restarts.
;;  :RESTART <n>, <n>  Invoke a restart with the given number.
;;  :FOO               Bar
;;
;; Any non-keyword non-integer form is evaluated.
")

(defun run-repl-command (input-string &optional condition)
  (with-input-from-string (input input-string)
    (with-output-to-string (output)
      (let ((stream (make-two-way-stream input output)))
        (portable-condition-system::read-eval-print-command
         stream condition)))))

(deftest debugger.repl.1
  (run-repl-command "(+ 2 2)")
  #.(format nil "[0] Debug> ~%4"))

(deftest debugger.repl.2
  (run-repl-command ":eval (+ 2 2)")
  #.(format nil "[0] Debug> ~%4"))
