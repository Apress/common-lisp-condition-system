;;;; integration/test.lisp

(uiop:define-package #:portable-condition-system.integration/test
  (:use #:common-lisp+portable-condition-system
        #:portable-condition-system.integration)
  (:export #:run))

(in-package #:portable-condition-system.integration/test)

(defun run ()
  (error "TODO"))
