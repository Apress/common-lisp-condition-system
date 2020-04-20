;;;; t/package.lisp

(uiop:define-package #:portable-condition-system/test
  (:use #:common-lisp+portable-condition-system)
  (:export #:run))

(in-package #:portable-condition-system/test)

(deftype program-error-type ()
  "The symbol naming the program error condition type that is signaled from
attempts to execute invalid Common Lisp code. Modify if the Common Lisp compiler
signals a different condition instead."
  'cl:program-error)
