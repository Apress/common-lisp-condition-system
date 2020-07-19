;;;; clcs-98-handler-bind-case.lisp

(defpackage #:clcs-98-handler-bind-case
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3))

(in-package #:clcs-98-handler-bind-case)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-handler-bind-case-with-no-error-case (form cases)
    (let* ((no-error-case (assoc :no-error cases))
           (other-cases (remove no-error-case cases)))
      (let ((normal-return (gensym "NORMAL-RETURN"))
            (error-return  (gensym "ERROR-RETURN")))
        `(block ,error-return
           (multiple-value-call (lambda ,@(cdr no-error-case))
             (block ,normal-return
               (return-from ,error-return
                 (handler-bind-case (return-from ,normal-return ,form)
                                    ,@other-cases))))))))

  (defun make-handler-bind-case-without-no-error-case (form cases)
    (let ((block-name (gensym "HANDLER-BIND-CASE-BLOCK")))
      (flet ((make-handler-binding (case)
               (destructuring-bind (type lambda-list . body) case
                 `(,type (lambda ,lambda-list
                           (return-from ,block-name (locally ,@body)))))))
        (let ((bindings (mapcar #'make-handler-binding cases)))
          `(block ,block-name (handler-bind ,bindings ,form))))))

  (defun expand-handler-bind-case (form cases)
    (let ((no-error-case-count (count :no-error cases :key #'car)))
      (case no-error-case-count
        (0 (make-handler-bind-case-without-no-error-case form cases))
        (1 (make-handler-bind-case-with-no-error-case form cases))
        (t (error "Multiple :NO-ERROR cases found in HANDLER-BIND-CASE."))))))

(defmacro handler-bind-case (form &body cases)
  (expand-handler-bind-case form cases))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-handler-bind-case*-with-no-error-case (form cases)
    (let* ((no-error-case (assoc :no-error cases))
           (other-cases (remove no-error-case cases)))
      (let ((normal-return (gensym "NORMAL-RETURN"))
            (error-return  (gensym "ERROR-RETURN")))
        `(block ,error-return
           (multiple-value-call (lambda ,@(cdr no-error-case))
             (block ,normal-return
               (return-from ,error-return
                 (handler-bind-case* (return-from ,normal-return ,form)
                                     ,@other-cases))))))))

  (defun make-handler-bind-case*-without-no-error-case (form cases)
    (if (null cases)
        form
        `(handler-bind-case (handler-bind-case* ,form ,@(cdr cases))
                            ,(car cases))))

  (defun expand-handler-bind-case* (form cases)
    (let ((no-error-case-count (count :no-error cases :key #'car)))
      (case no-error-case-count
        (0 (make-handler-bind-case*-without-no-error-case form cases))
        (1 (make-handler-bind-case*-with-no-error-case form cases))
        (t (error "Multiple :NO-ERROR cases found in HANDLER-BIND-CASE*."))))))

(defmacro handler-bind-case* (form &body cases)
  (expand-handler-bind-case* form cases))

(defun test-1 ()
  (handler-case (unwind-protect (signal 'condition)
                  (format t ";; Going out of dynamic scope~%"))
    (condition (c) (format t ";; Handling condition ~S~%" c))))

(defun test-2 ()
  (handler-bind-case (unwind-protect (signal 'condition)
                       (format t ";; Going out of dynamic scope~%"))
    (condition (c) (format t ";; Handling condition ~S~%" c))))

(defun test-3 ()
  (handler-bind-case* (unwind-protect (signal 'condition)
                        (format t ";; Going out of dynamic scope~%"))
    (condition (c)
               (format t ";; Handling condition ~S with A~%" c)
               (signal c))
    (condition (c) (format t ";; Handling condition ~S with B~%" c)
               (signal c))
    (condition (c) (format t ";; Handling condition ~S with C~%" c)
               (signal c))))

#|

CL-USER> (clcs-98-handler-bind-case:test-1)
;; Going out of dynamic scope
;; Handling condition #<CONDITION {101066A843}>
NIL

CL-USER> (clcs-98-handler-bind-case:test-2)
;; Handling condition #<CONDITION {10106AFA03}>
;; Going out of dynamic scope
NIL

CL-USER> (clcs-98-handler-bind-case:test-3)
;; Handling condition #<CONDITION {100FF606F3}> with C
;; Handling condition #<CONDITION {100FF606F3}> with B
;; Handling condition #<CONDITION {100FF606F3}> with A
;; Going out of dynamic scope
NIL

|#
