;;;; clcs-97-call-with-handler-restart-cached.lisp

(defpackage #:clcs-97-call-with-handler-restart-cached
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-97-call-with-handler-restart-cached)

(defvar *call-with-handler-cache* (make-hash-table :test #'equal))

(defvar *call-with-restart-cache* (make-hash-table :test #'equal))

(defun ensure-call-with-handler-function (condition-type)
  (multiple-value-bind (value foundp) (gethash condition-type *call-with-handler-cache*)
    (if foundp
        value
        (let ((lambda-form
                `(lambda (handler thunk)
                   (handler-bind ((,condition-type handler))
                     (funcall thunk)))))
          (setf (gethash condition-type *call-with-handler-cache*)
                (coerce lambda-form 'function))))))

(defun call-with-handler (thunk condition-type handler)
  (funcall (ensure-call-with-handler-function condition-type)
           handler thunk))

(defun ensure-call-with-restart-function
    (restart-name interactive-p report-p test-p)
  (let ((key (list restart-name interactive-p report-p test-p)))
    (multiple-value-bind (value foundp) (gethash key *call-with-restart-cache*)
      (if foundp
          value
          (let ((lambda-form
                  `(lambda (restart-function thunk interactive report test)
                     (declare (ignorable interactive report test))
                     (restart-bind
                         ((,restart-name
                            restart-function
                            ,@(when interactive-p `(:interactive-function interactive))
                            ,@(when report-p `(:report-function report))
                            ,@(when test-p `(:test-function test))))
                       (funcall thunk)))))
            (setf (gethash key *call-with-restart-cache*)
                  (coerce lambda-form 'function)))))))

(defun call-with-restart (thunk restart-name restart-function
                          &key (interactive-function nil interactive-p)
                            (report-function nil report-p)
                            (test-function nil test-p))
  (let ((function (ensure-call-with-restart-function
                   restart-name (and interactive-p t) (and report-p t) (and test-p t))))
    (funcall function restart-function thunk
             interactive-function report-function test-function)))

(defun test-1 ()
  (block foo
    (call-with-handler
     (lambda () (signal 'error))
     'error
     (lambda (c) (return-from foo c)))))

(defun test-2 ()
  (flet ((test (&rest args)
           (block foo
             (apply #'call-with-restart
                    (lambda () (invoke-restart 'foo))
                    'foo
                    (lambda () (return-from foo t))
                    args))))
    (list (test)
          (test :test-function (constantly t))
          (test :interactive-function (constantly nil))
          (test :test-function (constantly t)
                :interactive-function (constantly nil))
          (test :report-function (lambda (s) (write-string "foo" s)))
          (test :test-function (constantly t)
                :report-function (lambda (s) (write-string "foo" s)))
          (test :interactive-function (constantly nil)
                :report-function (lambda (s) (write-string "foo" s)))
          (test :test-function (constantly t)
                :interactive-function (constantly nil)
                :report-function (lambda (s) (write-string "foo" s))))))

#|

CL-USER> (clcs-97-call-with-handler-restart-cached:test-1)
#<ERROR {1005C832C3}>

CL-USER> (clcs-97-call-with-handler-restart-cached:test-2)
(T T T T T T T T)

|#
