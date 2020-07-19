;;;; clcs-11-handler-3-only-call-parents-maybe.lisp

(defpackage #:clcs-11-handler-3-only-call-parents-maybe
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-11-handler-3-only-call-parents-maybe)

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

(defun maybe-call-parent (condition)
  (let ((person (person condition)))
    (when (member :parent person)
      (when (= 0 (random 2))
        (format t ";; Nah, not calling ~A this time.~%" (first person))
        (throw :nope nil)))))

(defun skip-non-parents (condition)
  (let ((person (person condition)))
    (unless (member :parent person)
      (throw :nope nil))))

(defun test ()
  (handler-bind ((before-call #'maybe-call-parent)
                 (before-call #'skip-non-parents))
    (call-people)))

#|

CL-USER> (clcs-11-handler-3-only-call-parents-maybe:test)
;; Nah, not calling MOM this time.
;; Calling DAD.
NIL

;;;;;;;;;;; Or...
;; Calling MOM.
;; Calling DAD.
NIL

;;;;;;;;;;; Or...
;; .........

|#
