;;;; clcs-98-handler-case-star.lisp

(defpackage #:clcs-98-handler-case-star
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-98-handler-case-star)

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
    (let ((block-name (gensym "HANDLER-CASE*-BLOCK")))
      (flet ((make-handler-binding (case)
               (destructuring-bind (type lambda-list . body) case
                 `(,type (lambda ,lambda-list
                           (return-from ,block-name (locally ,@body)))))))
        (let ((bindings (mapcar #'make-handler-binding cases)))
          `(block ,block-name (handler-bind ,bindings ,form))))))

  (defun expand-handler-case (form cases)
    (let ((no-error-case-count (count :no-error cases :key #'car)))
      (case no-error-case-count
        (0 (make-handler-case*-without-no-error-case form cases))
        (1 (make-handler-case*-with-no-error-case form cases))
        (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE*."))))))

(defmacro handler-case* (form &rest cases)
  (expand-handler-case form cases))

(defun test-1 ()
  (handler-case (unwind-protect (signal 'condition)
                  (format t ";; Going out of dynamic scope~%"))
    (condition (c) (format t ";; Handling condition ~S~%" c))))

(defun test-2 ()
  (handler-case* (unwind-protect (signal 'condition)
                   (format t ";; Going out of dynamic scope~%"))
    (condition (c) (format t ";; Handling condition ~S~%" c))))

#|

CL-USER> (clcs-98-handler-case-star:test-1)
;; Going out of dynamic scope
;; Handling condition #<CONDITION {101066A843}>
NIL

CL-USER> (clcs-98-handler-case-star:test-2)
;; Handling condition #<CONDITION {10106AFA03}>
;; Going out of dynamic scope
NIL

|#
