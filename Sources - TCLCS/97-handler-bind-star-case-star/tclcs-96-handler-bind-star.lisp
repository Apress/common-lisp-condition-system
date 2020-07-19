;;;; clcs-96-handler-bind-star.lisp

(defpackage #:clcs-96-handler-bind-star
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-96-handler-bind-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-bind* (bindings body)
    (if (null bindings)
        `(progn ,@body)
        `(handler-bind (,(car bindings))
           (handler-bind* ,(cdr bindings) ,@body)))))

(defmacro handler-bind* (bindings &body body)
  (expand-handler-bind* bindings body))

(defun test-1 ()
  (handler-bind ((condition (lambda (c)
                              (format t ";; A~%")
                              (signal c)))
                 (condition (lambda (c)
                              (format t ";; B~%")
                              (signal c)))
                 (condition (lambda (c)
                              (format t ";; C~%")
                              (signal c))))
    (signal 'condition)))

(defun test-2 ()
  (handler-bind* ((condition (lambda (c)
                               (format t ";; A~%")
                               (signal c)))
                  (condition (lambda (c)
                               (format t ";; B~%")
                               (signal c)))
                  (condition (lambda (c)
                               (format t ";; C~%")
                               (signal c))))
    (signal 'condition)))

#|

CL-USER> (clcs-96-handler-bind-star:test-1)
;; A
;; B
;; C
NIL

CL-USER> (clcs-96-handler-bind-star:test-2)
;; C
;; B
;; A
;; A
;; B
;; A
;; A
NIL

|#
