;;;; integration.lisp

(uiop:define-package #:portable-condition-system/integration
  (:use #:common-lisp+portable-condition-system
        #:trivial-custom-debugger)
  (:export #:install))

(in-package #:portable-condition-system/integration)

;;; FOREIGN-CONDITION

(defun foreign-condition-report (condition stream)
  (format stream "Foreign condition ~S was signaled:~%~A"
          (type-of condition) (foreign-condition-condition condition)))

(define-condition foreign-condition ()
  ((condition :reader foreign-condition-condition :initarg :condition))
  (:default-initargs :condition (error "CONDITION required."))
  (:report foreign-condition-report))

(define-condition foreign-warning (foreign-condition warning) ())

(define-condition foreign-error (foreign-condition error) ())

(defun cl-condition-to-pcs (condition)
  (let ((type (etypecase condition
                (cl:error 'foreign-error)
                (cl:warning 'foreign-warning)
                (cl:condition 'foreign-condition))))
    (make-condition type :condition condition)))

;;; Integration

(defun debugger (condition hook)
  (let ((*debugger-hook* hook))
    (invoke-debugger condition)))

(defmethod invoke-debugger ((condition cl:condition))
  (let ((condition (cl-condition-to-pcs condition)))
    (signal condition)
    (portable-condition-system::standard-debugger condition)))

(defun install ()
  (install-debugger #'invoke-debugger-hook))
