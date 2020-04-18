;;;; package.lisp


(defpackage #:portable-condition-system/install
  (:use :cl))

(in-package #:portable-condition-system/install)

;;; TODO verify all symbols are implemented
(defmacro shadowed-symbols ()
  `(append
    '(;; Restarts and related functions
      #:restart #:restart-name #:find-restart #:compute-restarts
      #:invoke-restart #:invoke-restart-interactively
      ;; Condition-restart association
      #:with-condition-restarts
      ;; Restart macros
      #:restart-case #:restart-bind #:with-simple-restart)
    '(;; Defining and making conditions
      #:define-condition #:make-condition)
    '(;; Basic condition types
      #:condition #:warning #:serious-condition #:error
      ;; Simple condition
      #:simple-condition #:simple-warning #:simple-error
      #:simple-condition-format-control #:simple-condition-format-arguments
      ;; Storage condition
      #:storage-condition
      ;; Type error
      #:type-error #:type-error-datum #:type-error-expected-type
      #:simple-type-error
      ;; Control flow errors
      #:control-error #:program-error
      ;; Cell errors
      #:cell-error #:cell-error-name #:unbound-variable #:undefined-function
      ;; Unbound slot
      #:unbound-slot #:unbound-slot-instance
      ;; Stream errors
      #:stream-error #:stream-error-stream #:end-of-file
      ;; Parse errors
      #:parse-error #:reader-error
      ;; Package error
      #:package-error #:package-error-package
      ;; Arithmetic errors
      #:arithmetic-error #:arithmetic-error-operands
      #:arithmetic-error-operation #:division-by-zero
      #:floating-point-invalid-operation #:floating-point-inexact
      #:floating-point-overflow #:floating-point-underflow
      ;; File error
      #:file-error #:file-error-pathname
      ;; Print not readable
      #:print-not-readable #:print-not-readable-object)
    '(;; Assertions
      #:assert #:check-type #:etypecase #:ctypecase #:ecase #:ccase
      ;; Condition signaling
      #:signal #:warn #:cerror #:error #:cerror
      ;; Handler macros
      #:handler-case #:handler-bind #:ignore-errors)
    '(;; Debugger invocation
      #:break #:*break-on-signals* #:invoke-debugger #:*debugger-hook*)
    '(;; Standard restarts
      #:abort #:continue #:muffle-warning #:store-value #:use-value)))

(defmacro generate-defpackages ()
  (let ((symbols (shadowed-symbols)))
    `(progn
       (uiop:define-package #:portable-condition-system
         (:use #:common-lisp)
         (:shadow ,@symbols)
         (:export ,@symbols))
       (uiop:define-package #:common-lisp+portable-condition-system
         (:mix #:portable-condition-system #:common-lisp)
         (:reexport #:portable-condition-system #:common-lisp)))))

(generate-defpackages)

(in-package #:portable-condition-system)
