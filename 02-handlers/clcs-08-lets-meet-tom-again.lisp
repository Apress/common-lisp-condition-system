;;;; clcs-08-lets-meet-tom-again.lisp

(defpackage #:clcs-08-lets-meet-tom-again
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-08-lets-meet-tom-again)

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

(defun call-people ()
  (dolist (person *phonebook*)
    (call-person person)))

(defun test ()
  (call-people))

#|

CL-USER> (clcs-08-lets-meet-tom-again:test)
;; Calling MOM.
;; Calling DAD.
;; Calling ALICE.
;; Calling BOB.
;; Calling CATHERINE.
;; Calling DOROTHY.
;; Calling ERIC.
;; Calling DENTIST.
NIL

|#
