;;;; clcs-99-macros-101.lisp

(defpackage #:clcs-99-macros-101
  (:use #:cl)
  (:export #:my-and #:test-1a #:test-1b #:test-2 #:test-3 #:test-4 #:test-5 #:test-6))

(in-package #:clcs-99-macros-101)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01 - recursive AND macro

(defmacro my-and (&rest forms)
  (cond ((null forms) 'nil)
        ((null (rest forms)) (first forms))
        (t (list 'if (first forms) (cons 'my-and (rest forms))))))

(defun test-1a ()
  (macroexpand-1 '(my-and (= 0 (random 6)) (error "Bang!"))))

(defun test-1b ()
  (my-and (= 0 (random 6)) (error "Bang!")))

#|

CL-USER> (clcs-99-macros-101:test-1)
(IF (= 0 (RANDOM 6))
    (MY-AND (ERROR "Bang!")))
T

CL-USER> (clcs-99-macros-101:test-2)
NIL

;;; Or...

CL-USER> (clcs-99-macros-101:test-2)
;;; Error: Bang!

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 02 - backquote notation

(defun test-2 ()
  (let ((values '(1 2 3)))
    (list '(funcall function values)
          `(funcall function values)
          `(funcall function ,values)
          `(funcall function ,@values))))

#|

CL-USER> (clcs-99-macros-101:test-3)
((FUNCALL FUNCTION VALUES)
 (FUNCALL FUNCTION VALUES)
 (FUNCALL FUNCTION (1 2 3))
 (FUNCALL FUNCTION 1 2 3))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 03 - FOR with symbol capture

(defmacro for-3 ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(defun test-3 ()
  (let ((limit 10))
    (for-3 (i 0 3)
      (format t ";; ~D ~D~%" i limit))))

#|

CL-USER> (clcs-99-macros-101:test-3)
;; 0 3
;; 1 3
;; 2 3
;; 3 3
NIL

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 04 - FOR with wrong evaluation order

(defmacro for-4 ((var start stop) &body body)
  (let ((limit (gensym "LIMIT")))
    `(do ((,limit ,stop)
          (,var ,start (1+ ,var)))
         ((> ,var ,limit))
       ,@body)))

(defun test-4 ()
  (flet ((return-0 () (format t ";; Returning 0!~%") 0)
         (return-3 () (format t ";; Returning 3!~%") 3))
    (for-4 (i (return-0) (return-3))
      (format t ";; ~D~%" i))))

#|

CL-USER> (clcs-99-macros-101:test-4)
;; Returning 3!
;; Returning 0!
;; 0
;; 1
;; 2
;; 3
NIL

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 05 - FOR with multiple evaluation

(defmacro for-5 ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

(defun test-5 ()
  (flet ((return-0 () (format t ";; Returning 0!~%") 0)
         (return-3 () (format t ";; Returning 3!~%") 3))
    (for-5 (i (return-0) (return-3))
      (format t ";; ~D~%" i))))

#|

CL-USER> (clcs-99-macros-101:test-5)
;; Returning 0!
;; Returning 3!
;; 0
;; Returning 3!
;; 1
;; Returning 3!
;; 2
;; Returning 3!
;; 3
;; Returning 3!
NIL

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 06 - FOR, correct version

(defmacro for-6 ((var start stop) &body body)
  (let ((limit (gensym "LIMIT")))
    `(do ((,var ,start (1+ ,var))
          (,limit ,stop))
         ((> ,var ,limit))
       ,@body)))

(defun test-6 ()
  (let ((limit 10))
    (flet ((return-0 () (format t ";; Returning 0!~%") 0)
           (return-3 () (format t ";; Returning 3!~%") 3))
      (for-6 (i (return-0) (return-3))
        (format t ";; ~D ~D~%" i limit)))))

#|

CL-USER> (clcs-99-macros-101:test-6)
;; Returning 0!
;; Returning 3!
;; 0 10
;; 1 10
;; 2 10
;; 3 10
NIL

|#
