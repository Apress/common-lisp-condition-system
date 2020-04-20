;;;;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 27 22:36:48 2003
;;;; Contains: Tests of CELL-ERROR-NAME

(in-package #:portable-condition-system/test)

(deftest cell-error-name.3
  (cell-error-name (make-condition 'unbound-variable :name 'x))
  x)

(deftest cell-error-name.4
  (cell-error-name (make-condition 'undefined-function :name 'f))
  f)

(deftest cell-error-name.5
  (cell-error-name (make-condition 'unbound-slot :name 's))
  s)

(deftest cell-error-name.6
  (let ((i 0))
    (values
     (cell-error-name (progn (incf i) (make-condition
                                       'unbound-slot :name 's)))
     i))
  s 1)


;;; Need test raising condition unbound-slot


(deftest cell-error-name.error.1
  (signals-error (cell-error-name) cl:program-error)
  t)

(deftest cell-error-name.error.2
  (signals-error (cell-error-name (make-condition 'unbound-variable :name 'foo) nil)
                 cl:program-error)
  t)
