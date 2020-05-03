;;;; clcs-13-handler-6-calling-girlfriend-again.lisp

(defpackage #:clcs-13-handler-6-calling-girlfriend-again
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-13-handler-6-calling-girlfriend-again)

(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice :classmate :csgo :homework)
    (:bob :classmate :homework)
    (:catherine :classmate :ex)
    (:dorothy :classmate :girlfriend :csgo)
    (:eric :classmate :homework)
    (:dentist)))

(defun call-person (person)
  (format t ";; Calling ~A.~%" (first person)))

(defvar *csgo-launched-p* nil)

(define-condition before-call ()
  ((%person :reader person :initarg :person)))

(define-condition after-call ()
  ((%person :reader person :initarg :person)))

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :nope
      (signal 'before-call :person person)
      (call-person person)
      (signal 'after-call :person person))))

(defun ensure-csgo-launched (condition)
  (let ((person (person condition)))
    (when (member :csgo person)
      (unless *csgo-launched-p*
        (format t ";; Launching Counter Strike for ~A.~%" (first person))
        (setf *csgo-launched-p* t)))))

(defun call-girlfriend-again (condition)
  (let ((person (person condition)))
    (when (member :girlfriend person)
      (format t ";; Gonna call ~A again.~%" (first person))
      (call-person person))))

(defun test-1 ()
  (handler-bind ((after-call #'call-girlfriend-again))
    (call-people)))

(defun test-2 ()
  (handler-bind ((before-call #'ensure-csgo-launched)
                 (after-call #'call-girlfriend-again))
    (call-people)))

#|

CL-USER> (clcs-13-handler-6-calling-girlfriend-again:test-1)
;; Calling MOM.
;; Calling DAD.
;; Calling ALICE.
;; Calling BOB.
;; Calling CATHERINE.
;; Calling DOROTHY.
;; Gonna call DOROTHY again.
;; Calling DOROTHY.
;; Calling ERIC.
;; Calling DENTIST.
NIL

CL-USER> (clcs-13-handler-6-calling-girlfriend-again:test-2)
;; Calling MOM.
;; Calling DAD.
;; Launching Counter Strike for ALICE.
;; Calling ALICE.
;; Calling BOB.
;; Calling CATHERINE.
;; Calling DOROTHY.
;; Gonna call DOROTHY again.
;; Calling DOROTHY.
;; Calling ERIC.
;; Calling DENTIST.
NIL

|#
