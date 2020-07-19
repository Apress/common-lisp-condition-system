;;;; clcs-17-handler-clustering.lisp

(defpackage #:clcs-17-handler-clustering
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-17-handler-clustering)

(defun test ()
  (handler-bind ((condition (lambda (condition)
                              (declare (ignore condition))
                              (format t ";; Outer handler~%"))))
    (handler-bind ((condition (lambda (condition)
                                (declare (ignore condition))
                                (format t ";; Inner handler A~%")))
                   (condition (lambda (condition)
                                (format t ";; Inner handler B~%")
                                (signal condition)))
                   (condition (lambda (condition)
                                (declare (ignore condition))
                                (format t ";; Inner handler C~%"))))
      (signal 'condition))))

#|

CL-USER> (clcs-17-handler-clustering:test)
;; Inner handler A
;; Inner handler B
;; Outer handler
;; Inner handler C
;; Outer handler

|#
