;;;; clcs-14-exception-handling-1.lisp

(defpackage #:clcs-14-exception-handling-1
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-14-exception-handling-1)

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
    (signal 'grave-mistake :reason :about-to-call-your-ex)
    (we do not want to be here)))

(defun defuse-error (condition)
  (declare (ignore condition))
  (format t ";; Nope nope nope, not answering!~%")
  (throw :nopenopenope nil))

(defun defuse-grave-mistake (condition)
  (let ((reason (reason condition)))
    (format t ";; Nope nope nope, not answering - reason was, ~A!~%" reason))
  (throw :nopenopenope nil))

(defun test-1 ()
  (handler-bind ((error #'defuse-error))
    (dolist (person *phonebook*)
      (catch :nopenopenope
        (receive-phone-call person)))))

(defun test-2 ()
  (handler-bind ((grave-mistake #'defuse-grave-mistake))
    (dolist (person *phonebook*)
      (catch :nopenopenope
        (receive-phone-call person)))))

#|

CL-USER> (clcs-14-exception-handling-1:test-1)
;; Answering a call from MOM.
;; Answering a call from DAD.
;; Answering a call from ALICE.
;; Answering a call from BOB.
;; Answering a call from CATHERINE.
;; About to commit a grave mistake...
;; Nope nope nope, not answering!
;; Answering a call from DOROTHY.
;; Answering a call from ERIC.
;; Answering a call from DENTIST.
NIL

CL-USER> (clcs-14-exception-handling-1:test-2)
;; Answering a call from MOM.
;; Answering a call from DAD.
;; Answering a call from ALICE.
;; Answering a call from BOB.
;; Answering a call from CATHERINE.
;; About to commit a grave mistake...
;; Nope nope nope, not answering - reason was, ABOUT-TO-CALL-YOUR-EX!
;; Answering a call from DOROTHY.
;; Answering a call from ERIC.
;; Answering a call from DENTIST.
NIL

|#
