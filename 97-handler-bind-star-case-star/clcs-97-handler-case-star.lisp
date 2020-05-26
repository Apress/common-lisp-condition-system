;;;; clcs-97-handler-case-star.lisp

(defpackage #:clcs-97-handler-case-star
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-97-handler-case-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-handler-case*-with-no-error-case (form cases)
    (let* ((no-error-case (assoc :no-error cases))
           (other-cases (remove no-error-case cases)))
      (let ((normal-return (gensym "NORMAL-RETURN"))
            (error-return  (gensym "ERROR-RETURN")))
        `(block ,error-return
           (multiple-value-call (lambda ,@(cdr no-error-case))
             (block ,normal-return
               (return-from ,error-return
                 (handler-case* (return-from ,normal-return ,form)
                                ,@other-cases))))))))

  (defun make-handler-case*-without-no-error-case (form cases)
    (if (null cases)
        form
        `(handler-case (handler-case* ,form ,@(cdr cases))
           ,(car cases))))

  (defun expand-handler-case* (form cases)
    (let ((no-error-case-count (count :no-error cases :key #'car)))
      (case no-error-case-count
        (0 (make-handler-case*-without-no-error-case form cases))
        (1 (make-handler-case*-with-no-error-case form cases))
        (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE*."))))))

(defmacro handler-case* (form &body cases)
  (expand-handler-case* form cases))

(defun test-1 ()
  (handler-case (signal 'condition)
    (condition (c) (format t ";; A~%") (signal c))
    (condition (c) (format t ";; B~%") (signal c))
    (condition (c) (format t ";; C~%") (signal c))))

(defun test-2 ()
  (handler-case* (signal 'condition)
    (condition (c) (format t ";; A~%") (signal c))
    (condition (c) (format t ";; B~%") (signal c))
    (condition (c) (format t ";; C~%") (signal c))))

#|

CL-USER> (clcs-97-handler-case-star:test-1)
;; A
NIL

CL-USER> (clcs-97-handler-case-star:test-2)
;; C
;; B
;; A
NIL

|#
