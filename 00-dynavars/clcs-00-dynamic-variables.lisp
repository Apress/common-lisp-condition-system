;;;; clcs-00-dynamic-variables.lisp

(defpackage #:clcs-00-dynamic-variables
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4 #:test-5))

(in-package #:clcs-00-dynamic-variables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01 - lexical closure

(let ((x1 5))
  (defun foo1 ()
    x1))

(defun test-1 ()
  (values (foo1)))

;;; (clcs-00-dynamic-variables:test-1) ; -> 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 02 - lexical closure with indirection

(let ((x2 5))
  (defun bar2 ()
    x2)
  (defun foo2 ()
    (bar2)))

(defun test-2 ()
  (values (foo2)))

;;; (clcs-00-dynamic-variables:test-2) ; -> 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 03 - lexical closure with shadowing

(let ((x3 5))
  (defun bar3 ()
    x3)
  (defun foo3 ()
    (let ((x3 42))
      (bar3)))
  (defun quux3 ()
    (let ((x3 42))
      x3)))

(defun test-3 ()
  (values (foo3) (quux3)))

;;; (clcs-00-dynamic-variables:test-3) ; -> 5, 42

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 04 - dynamic rebinding of global dynamic variable

(defvar *x4* 5)

(defun bar4 ()
  *x4*)

(defun foo4 ()
  (let ((*x4* 42))
    (bar4)))

(defun test-4 ()
  (values (foo4) (bar4)))

;;; (clcs-00-dynamic-variables:test-4) ; -> 42, 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 05 - dynamic binding of local dynamic variable

(defun bar5 ()
  (declare (special *x5*))
  *x5*)

(defun foo5 ()
  (let ((*x5* 42))
    (declare (special *x5*))
    (bar5)))

(defun test-5 ()
  (values (foo5) (handler-case (bar5) (error (c) (type-of c)))))

;;; (clcs-00-dynamic-variables:test-5) ; -> 42, UNBOUND-VARIABLE
