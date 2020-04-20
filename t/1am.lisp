;;;; t/1am.lisp

(in-package #:portable-condition-system/test)

;;; This software contains parts of the 1AM testing framework, licensed
;;; under the following terms:
;;;
;;; Copyright (c) 2014 James M. Lawrence
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defvar *tests* nil)
(defvar *pass-count* nil)
(defvar *running* nil)

(defun report (test-count pass-count)
  (format t "~&~S test~:p, ~S check~:p.~%" test-count pass-count))

(defun %run (fn test-count)
  (let ((*pass-count* 0))
    (multiple-value-prog1 (funcall fn)
      (report test-count *pass-count*))))

(defun run (&optional (tests *tests*))
  "Run each test in the sequence `tests'. Default is `*tests*'."
  (let ((*running* t))
    (%run (lambda () (mapc #'funcall tests))
          (length tests)))
  (values))

(defun call-test (name fn)
  (format t "~&~s" name)
  (finish-output)
  (if *running*
      (funcall fn)
      (%run fn 1)))

(defmacro deftest (name body &rest expected-values)
  "Define a test function and add it to `*tests*'."
  `(progn
     (defun ,name ()
       (call-test ',name
                  (lambda () (is (equal ',expected-values
                                        (multiple-value-list ,body))))))
     (pushnew ',name *tests*)
     ',name))

(defun passed ()
  (write-char #\.)
  ;; Checks done outside a test run are not tallied.
  (when *pass-count*
    (incf *pass-count*))
  (values))

(defmacro is (form)
  "Assert that `form' evaluates to non-nil."
  `(progn
     (cl:with-simple-restart (fail "Continue running the test suite.")
       (cl:assert ,form))
     (passed)))

(defun %signals (expected fn)
  (flet ((handler (condition)
           (cond ((typep condition expected)
                  (passed)
                  (return-from %signals t))
                 (t (cl:error "Expected to signal ~s, but got ~s:~%~a"
                              expected (type-of condition) condition)))))
    (cl:handler-bind ((cl:condition #'handler))
      (handler-bind ((condition #'handler))
        (funcall fn))))
  (cl:error "Expected to signal ~s, but got nothing." expected))

(defmacro signals-error (form condition)
  "Assert that `body' signals a condition of type `condition'."
  `(%signals ',condition (lambda () ,form)))
