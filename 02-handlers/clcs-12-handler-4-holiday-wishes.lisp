;;;; clcs-12-handler-4-holiday-wishes.lisp

(defpackage #:clcs-12-handler-4-holiday-wishes
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-12-handler-4-holiday-wishes)

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

(defun skip-ex (condition)
  (let ((person (person condition)))
    (when (member :ex person)
      (throw :nope nil))))

(defun wish-happy-holidays (condition)
  (let ((person (person condition)))
    (format t ";; Gonna wish ~A happy holidays!~%" (first person))))

(defun test-1 ()
  (handler-bind ((before-call #'skip-ex)
                 (before-call #'wish-happy-holidays))
    (call-people)))

(defun test-2 ()
  (handler-bind ((before-call #'wish-happy-holidays))
    (handler-bind ((before-call #'skip-ex))
      (call-people))))

#|

CL-USER> (clcs-12-handler-4-holiday-wishes:test-1)
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

CL-USER> (clcs-12-handler-4-holiday-wishes:test-2)
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
