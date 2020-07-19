;;;; clcs-96-call-with-handler-restart.lisp

(defpackage #:clcs-96-call-with-handler-restart
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-96-call-with-handler-restart)

(defun call-with-handler (thunk condition-type handler)
  (let ((lambda-form
          `(lambda ()
             (handler-bind ((,condition-type ,handler))
               (funcall ,thunk)))))
    (funcall (coerce lambda-form 'function))))

(defun call-with-restart (thunk restart-name restart-function
                          &key (interactive-function nil interactive-function-p)
                            (test-function nil test-function-p)
                            (report-function nil report-function-p))
  (let ((lambda-form
          `(lambda ()
             (restart-bind ((,restart-name
                              ,restart-function
                              ,@(when interactive-function-p
                                  `(:interactive-function ,interactive-function))
                              ,@(when report-function-p
                                  `(:report-function ,report-function))
                              ,@(when test-function-p
                                  `(:test-function ,test-function))))
               (funcall ,thunk)))))
    (funcall (coerce lambda-form 'function))))

(defun test-1 ()
  (call-with-handler (lambda () (error "bar"))
                     'error
                     (lambda (c) (return-from test-1 c))))

(defun test-2 ()
  (call-with-restart (lambda () (invoke-restart 'frob))
                     'frob
                     (lambda () (return-from test-2 42))))

#|

CL-USER> (clcs-96-call-with-handler-restart:test-1)
#<SIMPLE-ERROR "bar" {1008678E03}>

CL-USER> (clcs-96-call-with-handler-restart:test-2)
42

|#
