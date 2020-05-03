;;;; clcs-02-hook-1-launching-csgo.lisp

(defpackage #:clcs-02-hook-1-launching-csgo
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-02-hook-1-launching-csgo)

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

(defvar *hooks* '())

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (dolist (hook *hooks*)
      (funcall hook person))
    (call-person person)))

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))

(defun test ()
  (let ((*hooks* (list #'ensure-csgo-launched)))
    (call-people)))

#|

CL-USER> (clcs-02-hook-1-launching-csgo:test)
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
