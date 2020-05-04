;;;; clcs-35-check-type.lisp

(defpackage #:clcs-35-check-type
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-35-check-type)

(defun test-1 ()
  (let ((x 42))
    (check-type x integer)))

(defun test-2 ()
  (let ((x "42"))
    (handler-case (check-type x integer)
      (type-error () :oops))))

(defun test-3 ()
  (let ((x "42"))
    (handler-bind ((type-error (lambda (condition)
                                 (declare (ignore condition))
                                 (store-value 42))))
      (check-type x integer)
      x)))

(defun test-4 ()
  (let ((x 24))
    (handler-case (check-type x (eql 42) "the ultimate answer to everything")
      (type-error (condition)
        (let ((*print-escape* nil))
          (format t ";; ~W~%" condition))))))

#|

CL-USER> (clcs-35-check-type:test-1)
NIL

CL-USER> (clcs-35-check-type:test-2)
:OOPS

CL-USER> (clcs-35-check-type:test-3)
42

CL-USER> (clcs-35-check-type:test-4)
;; The value of X is 24, which is not the ultimate answer to everything.
NIL

|#
