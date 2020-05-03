;;;; clcs-06-hook-5-calling-girlfriend-again.lisp

(defpackage #:clcs-06-hook-5-calling-girlfriend-again
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-06-hook-5-calling-girlfriend-again)

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

(defvar *before-hooks* '())

(defvar *after-hooks* '())

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :nope
      (dolist (hook *before-hooks*)
        (funcall hook person))
      (call-person person)
      (dolist (hook *after-hooks*)
        (funcall hook person)))))

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))

(defun call-girlfriend-again (person)
  (when (member :girlfriend person)
    (format t ";; Gonna call ~A again.~%" (first person))
    (call-person person)))

(defun test-1 ()
  (let ((*after-hooks* (list #'call-girlfriend-again)))
    (call-people)))

(defun test-2 ()
  (let ((*before-hooks* (list #'ensure-csgo-launched))
        (*after-hooks* (list #'call-girlfriend-again)))
    (call-people)))

#|

CL-USER> (clcs-06-hook-5-calling-girlfriend-again:test-1)
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

CL-USER> (clcs-06-hook-5-calling-girlfriend-again:test-2)
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
