;;;; clcs-01-lets-meet-tom.lisp

(defpackage #:clcs-01-lets-meet-tom
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-01-lets-meet-tom)

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

CL-USER> (clcs-01-lets-meet-tom:test)
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
