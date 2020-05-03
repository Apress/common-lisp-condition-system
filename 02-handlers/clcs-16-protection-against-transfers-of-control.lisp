;;;; clcs-16-protection-against-transfers-of-control.lisp

(defpackage #:clcs-16-protection-against-transfers-of-control
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-16-protection-against-transfers-of-control)

(defvar *phonebook*
  '((:bob :classmate :homework)
    (:catherine :classmate :ex)
    (:dorothy :classmate :girlfriend :csgo)))

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
    (catch :nopenopenope
      (handler-case (receive-phone-call person)
        (grave-mistake (e) (defuse-grave-mistake e)))
      (format t ";; Restarting phone.~%"))))

(defun test-2 ()
  (dolist (person *phonebook*)
    (catch :nopenopenope
      (unwind-protect
           (handler-case (receive-phone-call person)
             (grave-mistake (e) (defuse-grave-mistake e)))
        (format t ";; Restarting phone.~%")))))

#|

CL-USER> (clcs-16-protection-against-transfers-of-control:test-1)
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

CL-USER> (clcs-16-protection-against-transfers-of-control:test-2)
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
