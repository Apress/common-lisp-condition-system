;;;; clcs-15-exception-handling-2.lisp

(defpackage #:clcs-15-exception-handling-2
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-15-exception-handling-2)

(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice :classmate :csgo :homework)
    (:bob :classmate :homework)
    (:catherine :classmate :ex)
    (:dorothy :classmate :girlfriend :csgo)
    (:eric :classmate :homework)
    (:dentist)))

(define-condition grave-mistake (error)
  ((%reason :reader reason :initarg :reason)))

(defun receive-phone-call (person)
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (format t ";; About to commit a grave mistake...~%")
    (error 'grave-mistake :reason :about-to-call-your-ex)
    (we will never get here)))

(defun defuse-grave-mistake (condition)
  (let ((reason (reason condition)))
    (format t ";; Nope nope nope, not answering - reason was, ~A!~%" reason))
  (throw :nopenopenope nil))

(defun test-1 ()
  (dolist (person *phonebook*)
    (handler-case (receive-phone-call person)
      (grave-mistake () (format t ";; Nope, not this time.~%")))))

(defun test-2 ()
  (dolist (person *phonebook*)
    (ignore-errors (receive-phone-call person))))

#|

CL-USER> (clcs-15-exception-handling-2:test-1)
;; Answering a call from MOM.
;; Answering a call from DAD.
;; Answering a call from ALICE.
;; Answering a call from BOB.
;; Answering a call from CATHERINE.
;; About to commit a grave mistake...
;; Nope, not this time.
;; Answering a call from DOROTHY.
;; Answering a call from ERIC.
;; Answering a call from DENTIST.
NIL

CL-USER> (clcs-15-exception-handling-2:test-2)
;; Answering a call from MOM.
;; Answering a call from DAD.
;; Answering a call from ALICE.
;; Answering a call from BOB.
;; Answering a call from CATHERINE.
;; About to commit a grave mistake...
;; Answering a call from DOROTHY.
;; Answering a call from ERIC.
;; Answering a call from DENTIST.
NIL

|#
