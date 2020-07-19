;;;; clcs-09-handler-1-launching-csgo.lisp

(defpackage #:clcs-09-handler-1-launching-csgo
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-09-handler-1-launching-csgo)

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

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (signal 'before-call :person person)
    (call-person person)))

(defun ensure-csgo-launched (condition)
  (let ((person (person condition)))
    (when (member :csgo person)
      (unless *csgo-launched-p*
        (format t ";; Launching Counter Strike for ~A.~%" (first person))
        (setf *csgo-launched-p* t)))))

(defun test ()
  (handler-bind ((before-call #'ensure-csgo-launched))
    (call-people)))

#|

CL-USER> (clcs-09-handler-1-launching-csgo:test)
;; Calling MOM.
;; Calling DAD.
;; Launching Counter Strike for ALICE.
;; Calling ALICE.
;; Calling BOB.
;; Calling CATHERINE.
;; Calling DOROTHY.
;; Calling ERIC.
;; Calling DENTIST.
NIL

|#
