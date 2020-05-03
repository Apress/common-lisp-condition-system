;;;; clcs-05-hook-4-holiday-wishes.lisp

(defpackage #:clcs-05-hook-4-holiday-wishes
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-05-hook-4-holiday-wishes)

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

(defun skip-ex (person)
  (when (member :ex person)
    (throw :nope nil)))

(defun wish-happy-holidays (person)
  (format t ";; Gonna wish ~A happy holidays!~%" (first person)))

(defun test-1 ()
  (let ((*hooks* (list #'skip-ex
                       #'wish-happy-holidays)))
    (call-people)))

(defun test-2 ()
  (let ((*hooks* (list #'wish-happy-holidays)))
    (let ((*hooks* (append (list #'skip-ex) *hooks*)))
      (call-people))))

#|

CL-USER> (clcs-05-hook-4-holiday-wishes:test-1)
;; Gonna wish MOM happy holidays!
;; Calling MOM.
;; Gonna wish DAD happy holidays!
;; Calling DAD.
;; Gonna wish ALICE happy holidays!
;; Calling ALICE.
;; Gonna wish BOB happy holidays!
;; Calling BOB.
;; Gonna wish DOROTHY happy holidays!
;; Calling DOROTHY.
;; Gonna wish ERIC happy holidays!
;; Calling ERIC.
;; Gonna wish DENTIST happy holidays!
;; Calling DENTIST.
NIL

CL-USER> (clcs-05-hook-4-holiday-wishes:test-2)
;; Gonna wish MOM happy holidays!
;; Calling MOM.
;; Gonna wish DAD happy holidays!
;; Calling DAD.
;; Gonna wish ALICE happy holidays!
;; Calling ALICE.
;; Gonna wish BOB happy holidays!
;; Calling BOB.
;; Gonna wish DOROTHY happy holidays!
;; Calling DOROTHY.
;; Gonna wish ERIC happy holidays!
;; Calling ERIC.
;; Gonna wish DENTIST happy holidays!
;; Calling DENTIST.
NIL

|#
