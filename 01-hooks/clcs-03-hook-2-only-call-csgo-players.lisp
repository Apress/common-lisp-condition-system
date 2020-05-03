;;;; clcs-03-hook-2-only-call-csgo-players.lisp

(defpackage #:clcs-03-hook-2-only-call-csgo-players
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-03-hook-2-only-call-csgo-players)

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
    (catch :nope
      (dolist (hook *hooks*)
        (funcall hook person))
      (call-person person))))

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))

(defun skip-non-csgo-people (person)
  (unless (member :csgo person)
    (format t ";; Nope, not calling ~A.~%" (first person))
    (throw :nope nil)))

(defun test ()
  (let ((*hooks* (list #'ensure-csgo-launched
                       #'skip-non-csgo-people)))
    (call-people)))

#|

CL-USER> (clcs-03-hook-2-only-call-csgo-players:test)
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
