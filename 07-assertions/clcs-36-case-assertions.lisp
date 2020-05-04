;;;; clcs-36-case-assertions.lisp

(defpackage #:clcs-36-case-assertions
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-36-case-assertions)

(defun test-1 ()
  (let ((x 24))
    (handler-case (ecase x
                    (42 :integer)
                    (:forty-two :keyword)
                    ("42" :string))
      (type-error (condition)
        (let ((*print-escape* nil))
          (format t ";; ~W" condition))))))

(defun test-2 ()
  (let ((x nil))
    (handler-case (etypecase x
                    (integer :integer)
                    (keyword :keyword)
                    (string :string))
      (type-error (condition)
        (let ((*print-escape* nil))
          (format t ";; ~W" condition))))))

(defun test-3 ()
  (let ((x nil))
    (handler-bind ((type-error (lambda (condition)
                                 (declare (ignore condition))
                                 (store-value 42))))
      (ctypecase x
        (integer :integer)
        (keyword :keyword)
        (string :string)))))

(defun test-4 ()
  (let ((x nil))
    (handler-bind ((type-error (lambda (condition)
                                 (declare (ignore condition))
                                 (store-value :forty-two))))
      (ccase x
        (42 :integer)
        (:forty-two :keyword)
        ("42" :string)))))

#|

CL-USER> (clcs-36-case-assertions:test-1)
;; 24 fell through ECASE expression. Wanted one of (42 :FORTY-TWO "42").
NIL

CL-USER> (clcs-36-case-assertions:test-2)
;; NIL fell through ETYPECASE expression. Wanted one of (INTEGER KEYWORD STRING).
NIL

CL-USER> (clcs-36-case-assertions:test-3)
:INTEGER

CL-USER> (clcs-36-case-assertions:test-4)
:KEYWORD

|#
