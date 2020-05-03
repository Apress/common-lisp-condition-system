;;;; clcs-07-multiple-types-of-hooks.lisp

(defpackage #:clcs-07-multiple-types-of-hooks
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-07-multiple-types-of-hooks)

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

(defun call-hooks (kind &rest arguments)
  (dolist (hook *hooks*)
    (destructuring-bind (hook-kind hook-function) hook
      (when (eq kind hook-kind)
        (apply hook-function arguments)))))

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :nope
      (call-hooks 'before-call person)
      (call-person person)
      (call-hooks 'after-call person))))

(defun skip-ex (person)
  (when (member :ex person)
    (throw :nope nil)))

(defun wish-happy-holidays (person)
  (format t ";; Gonna wish ~A happy holidays!~%" (first person)))

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
  (let ((*hooks* `((before-call ,#'ensure-csgo-launched)
                   (after-call ,#'call-girlfriend-again))))
    (call-people)))

(defun test-2 ()
  (let ((*hooks* `((before-call ,#'skip-ex)
                   (before-call ,#'ensure-csgo-launched)
                   (before-call ,#'wish-happy-holidays)
                   (after-call ,#'call-girlfriend-again))))
    (call-people)))

#|

CL-USER> (clcs-07-multiple-types-of-hooks:test-1)
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

CL-USER> (clcs-07-multiple-types-of-hooks:test-2)
;; Gonna wish MOM happy holidays!
;; Calling MOM.
;; Gonna wish DAD happy holidays!
;; Calling DAD.
;; Launching Counter Strike for ALICE.
;; Gonna wish ALICE happy holidays!
;; Calling ALICE.
;; Gonna wish BOB happy holidays!
;; Calling BOB.
;; Gonna wish DOROTHY happy holidays!
;; Calling DOROTHY.
;; Gonna call DOROTHY again.
;; Calling DOROTHY.
;; Gonna wish ERIC happy holidays!
;; Calling ERIC.
;; Gonna wish DENTIST happy holidays!
;; Calling DENTIST.
NIL

|#
