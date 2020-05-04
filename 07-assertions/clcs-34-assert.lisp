;;;; clcs-34-assert.lisp

(defpackage #:clcs-34-assert
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4 #:test-5 #:test-6))

(in-package #:clcs-34-assert)

(defun test-1 ()
  (handler-case (assert (= (+ 2 2) 4))))

(defun test-2 ()
  (handler-case (assert (= (+ 2 2) 5))
    (error () :nope)))

(defun test-3 ()
  (handler-bind ((error (lambda (condition)
                          (declare (ignore condition))
                          (format t ";; Retrying...~%")
                          (continue))))
    (assert (= (random 10) 0))))

(defun test-4 ()
  (handler-bind ((error #'continue))
    (let ((x nil))
      (assert x (x)))))

(defun test-5 ()
  (handler-case (assert (= (+ 2 2) 5) ()
                        'type-error :datum 5 :expected-type '(eql 4))
    (error (condition)
      (let ((*print-escape* nil))
        (format t ";; ~W~%" condition)))))

(defun test-6 ()
  (handler-case (assert (= (+ 2 2) 5) ()
                        "The numbers ~D and ~D do not sum up to ~D." 2 2 5)
    (error (condition)
      (let ((*print-escape* nil))
        (format t ";; ~W~%" condition)))))

#|

CL-USER> (clcs-34-assert:test-1)
NIL

CL-USER> (clcs-34-assert:test-2)
:NOPE

CL-USER> (clcs-34-assert:test-3)
;; Retrying...
;; Retrying...
;; Retrying...
;; Retrying...
;; Retrying...                                                 ; the number of retries
;; Retrying...                                                 ; will vary randomly
;; Retrying...
;; Retrying...
;; Retrying...
;; Retrying...
NIL

CL-USER> (clcs-34-assert:test-4)
;; The old value of X is NIL.
;; Do you want to supply a new value? (y or n) y               ; user input here
;; Type a form to be evaluated: t                              ; user input here
NIL

CL-USER> (clcs-34-assert:test-5)
;; The value 5 is not of type (EQL 4).
NIL

CL-USER> (clcs-34-assert:test-6)
;; The numbers 2 and 2 do not sum up to 5.
NIL



|#
