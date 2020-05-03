;;;; clcs-10-handler-2-only-call-csgo-players.lisp

(defpackage #:clcs-10-handler-2-only-call-csgo-players
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-10-handler-2-only-call-csgo-players)

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
    (catch :nope
      (signal 'before-call :person person)
      (call-person person))))

(defun ensure-csgo-launched (condition)
  (let ((person (person condition)))
    (when (member :csgo person)
      (unless *csgo-launched-p*
        (format t ";; Launching Counter Strike for ~A.~%" (first person))
        (setf *csgo-launched-p* t)))))

(defun skip-non-csgo-people (condition)
  (let ((person (person condition)))
    (unless (member :csgo person)
      (format t ";; Nope, not calling ~A.~%" (first person))
      (throw :nope nil))))

(defun test ()
  (handler-bind ((before-call #'ensure-csgo-launched)
                 (before-call #'skip-non-csgo-people))
    (call-people)))

#|

CL-USER> (clcs-10-handler-2-only-call-csgo-players:test)
;; Nope, not calling MOM.
;; Nope, not calling DAD.
;; Launching Counter Strike for ALICE.
;; Calling ALICE.
;; Nope, not calling BOB.
;; Nope, not calling CATHERINE.
;; Calling DOROTHY.
;; Nope, not calling ERIC.
;; Nope, not calling DENTIST.
NIL

|#
