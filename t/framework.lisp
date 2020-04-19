;;;; t/framework.lisp

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

;;; Support code pulled out of ANSI-TEST

;;; TODO organize the below in some meaningful way

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(defmacro signals-type-error (var datum-form form &key (safety 3) (inline nil))
  (let ((lambda-form
          `(lambda (,var)
             (declare (optimize (safety ,safety)))
             ,form)))
    `(let ((,var ,datum-form))
       (declare (optimize safety))
       (handler-bind
           ((warning #'(lambda (c) (declare (ignore c))
                         (muffle-warning))))
         (handler-case
             (apply #'values
                    nil
                    (multiple-value-list
                     (funcall
                      ,(cond
                         (inline `(function ,lambda-form))
                         (t `(eval ',lambda-form)))
                      ,var)))
           (type-error
             (c)
             (let ((datum (type-error-datum c))
                   (expected-type (type-error-expected-type c)))
               (cond
                 ((not (eql ,var datum))
                  (list :datum-mismatch ,var datum))
                 ((typep datum expected-type)
                  (list :is-typep datum expected-type))
                 (t (printable-p c))))))))))


(defun printable-p (obj)
  "Returns T iff obj can be printed to a string."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-escape* nil))
      (declare (optimize safety))
      (handler-case (and (stringp (write-to-string obj)) t)
        (condition (c) (declare (ignore c)) nil)))))

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Try out the format control and format arguments of a simple-condition C,
   but make no assumptions about what they print as, only that they
   do print."
  (declare (ignore expected-fmt expected-args))
  (and (typep c 'simple-condition)
       (let ((fc (simple-condition-format-control c))
             (args (simple-condition-format-arguments c)))
         (and
          (stringp (apply #'format nil fc args))
          t))))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eq x y)))))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))

(defun notnot (x) (not (not x)))

(defmacro notnot-mv (form)
  `(notnot-mv-fn (multiple-value-list ,form)))

(defun notnot-mv-fn (results)
  (if (null results)
      (values)
      (apply #'values
             (not (not (first results)))
             (rest results))))

(defparameter *condition-types*
  '(arithmetic-error
    cell-error
    condition
    control-error
    division-by-zero
    end-of-file
    error
    file-error
    floating-point-inexact
    floating-point-invalid-operation
    floating-point-underflow
    floating-point-overflow
    package-error
    parse-error
    print-not-readable
    program-error
    reader-error
    serious-condition
    simple-condition
    simple-error
    simple-type-error
    simple-warning
    storage-condition
    stream-error
    style-warning
    type-error
    unbound-slot
    unbound-variable
    undefined-function
    warning))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare (optimize safety))
    (ignore-errors
     (setf (logical-pathname-translations "PCSTEST")
           `(("**;*.*.*"
              ,(merge-pathnames
                "t/"
                (make-pathname :directory '(:absolute :wild-inferiors)
                               :name :wild :type :wild))))))))

(defstruct pcs-test-struct)

(defparameter *mini-universe*
  (remove-duplicates
   (append
    (list 'a 2 #\a "a" '(a . a) (make-instance 'condition)
          (find-package :cl) #(a) (make-hash-table) #p""
          (logical-pathname "PCSTEST:") *standard-input*
          (make-pcs-test-struct) #'identity *random-state*
          (first (compute-applicable-methods
                  #'print-object
                  (list 2 *standard-output*)))
          1.2s0 1.3f0 1.5d0 1.8l0 3/5 10000000000000000000000))))


(defun check-type-error* (pred-fn guard-fn &optional (universe *mini-universe*))
  "Check that for all elements in some set, either guard-fn is true or
   pred-fn signals a type error."
  (let (val)
    (loop for e in universe
          unless (or (funcall guard-fn e)
                     (equal
                      (setf val (multiple-value-list
                                 (signals-type-error x e (funcall pred-fn x)
                                                     :inline t)))
                      '(t)))
            collect (list e val))))

(defmacro check-type-error (&body args)
  `(locally (declare (optimize safety)) (check-type-error* ,@args)))

(defun make-def-cond-name (name &rest suffixes)
  (intern (apply #'concatenate 'string (string name) "/"
                 (mapcar #'string suffixes))
          '#:portable-condition-system/test))

(defmacro define-condition-with-tests (name-symbol
                                       parents slot-specs &rest options)
  "Create a condition and some associated tests."
  (assert (symbolp name-symbol))
  (dolist (parent parents) (assert (symbolp parent)))
  (let ((name (symbol-name name-symbol)))
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       (report-and-ignore-errors (eval '(define-condition ,name-symbol ,parents
                                         ,slot-specs ,@options)))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF/" parent)
                  (subtypep* ',name-symbol ',parent)
                  t t))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF-2/" parent)
                  (check-all-subtypep ',name-symbol ',parent)
                  nil))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name
                                              "IS-NOT-SUPERTYPE-OF/" parent)
                  (subtypep* ',parent ',name-symbol)
                  nil t))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name "IS-A/" parent)
                  (let ((c (make-condition ',name-symbol)))
                    (notnot-mv (typep c ',parent)))
                  t))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name "IS-SUBCLASS-OF/" parent)
                  (subtypep* (find-class ',name-symbol)
                             (find-class ',parent))
                  t t))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name
                                              "IS-NOT-SUPERCLASS-OF/" parent)
                  (subtypep* (find-class ',parent)
                             (find-class ',name-symbol))
                  nil t))
       ,@(loop for parent in (adjoin 'condition parents)
               collect
               `(deftest ,(make-def-cond-name name "IS-A-MEMBER-OF-CLASS/"
                                              parent)
                  (let ((c (make-condition ',name-symbol)))
                    (notnot-mv (typep c (find-class ',parent))))
                  t))
       (deftest ,(make-def-cond-name name "HANDLER-CASE-1")
         (let ((c (make-condition ',name-symbol)))
           (handler-case (normally (signal c))
             (,name-symbol (c1) (eqt c c1))))
         t)
       (deftest ,(make-def-cond-name name "HANDLER-CASE-2")
         (let ((c (make-condition ',name-symbol)))
           (handler-case (normally (signal c))
             (condition (c1) (eqt c c1))))
         t)
       ,@(unless (some #'(lambda (ct) (subtypep ct 'error)) parents)
           `((deftest ,(make-def-cond-name name "HANDLER-CASE-3")
               (let ((c (make-condition ',name-symbol)))
                 (handler-case (normally (signal c))
                   (error () nil)
                   (,name-symbol (c2) (eqt c c2))))
               t))))))

(defparameter *report-and-ignore-errors-break* nil
  "When true, REPORT-AND-IGNORE-ERRORS breaks instead of discarding the error
condition.")

(defmacro report-and-ignore-errors (&body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (#+sbcl let #+sbcl () #-sbcl progn
      (handler-case
          (progn ,@body)
        (error (condition)
          (princ condition)
          (terpri)
          (when *report-and-ignore-errors-break* (break))
          (values nil condition))))))

(defun subtypep* (type1 type2)
  (apply #'values
         (mapcar #'notnot
                 (multiple-value-list (subtypep type1 type2)))))

(defvar *should-always-be-true* t)

(declaim (notinline should-never-be-called))

(defun should-never-be-called () nil)

(defmacro normally (form &optional (default-form
                                    '(should-never-be-called)))
  `(if *should-always-be-true* ,form ,default-form))

(defun check-all-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defun check-subtypep (type1 type2 is-sub &optional should-be-valid)
  (multiple-value-bind
        (sub valid)
      (subtypep type1 type2)
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid sub (not is-sub))
            (and valid (not sub) is-sub)
            (and (not valid) should-be-valid))
        `(((SUBTYPEP ,type1 ,type2) :==> ,sub ,valid))
        nil)))

(defun check-equivalence (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep type2 type1 t)
   (check-subtypep `(not ,type1) `(not ,type2) t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep `(and ,type2 (not ,type1)) nil t)
   (check-subtypep `(and (not ,type2) ,type1) nil t)
   (check-subtypep `(and (not ,type1) ,type2) nil t)
   (check-subtypep t `(or ,type1 (not ,type2)) t)
   (check-subtypep t `(or ,type2 (not ,type1)) t)
   (check-subtypep t `(or (not ,type2) ,type1) t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defparameter *cl-type-symbols*
  '(
    atom
    base-char
    base-string
    bignum
    bit
    boolean
    compiled-function
    extended-char
    fixnum
    keyword
    nil
    short-float
    single-float
    double-float
    long-float
    signed-byte
    simple-array
    simple-base-string
    simple-bit-vector
    simple-string
    simple-vector
    standard-char
    unsigned-byte
    ))

(defparameter *cl-types-that-are-classes-symbols*
  '(
    arithmetic-error
    array
    bit-vector
    broadcast-stream
    built-in-class
    cell-error
    character
    class
    complex
    concatenated-stream
    condition
    cons
    control-error
    division-by-zero
    echo-stream
    end-of-file
    error
    file-error
    file-stream
    float
    floating-point-inexact
    floating-point-invalid-operation
    floating-point-overflow
    floating-point-underflow
    function
    generic-function
    hash-table
    integer
    list
    logical-pathname
    method
    method-combination
    null
    number
    package
    package-error
    parse-error
    pathname
    print-not-readable
    program-error
    random-state
    ratio
    rational
    reader-error
    readtable
    real
    restart
    sequence
    serious-condition
    simple-condition
    simple-error
    simple-type-error
    simple-warning
    standard-class
    standard-generic-function
    standard-method
    standard-object
    storage-condition
    stream
    stream-error
    string
    string-stream
    structure-class
    structure-object
    style-warning
    symbol
    synonym-stream
    t
    two-way-stream
    type-error
    unbound-slot
    unbound-variable
    undefined-function
    vector
    warning
    ))

(defparameter *cl-system-class-symbols*
  '(
    array
    bit-vector
    broadcast-stream
    built-in-class
    character
    class
    complex
    concatenated-stream
    cons
    echo-stream
    file-stream
    float
    function
    generic-function
    hash-table
    integer
    list
    logical-pathname
    method
    method-combination
    null
    number
    package
    pathname
    random-state
    ratio
    rational
    readtable
    real
    restart
    sequence
    standard-class
    standard-generic-function
    standard-method
    stream
    string
    string-stream
    structure-class
    symbol
    synonym-stream
    t
    two-way-stream
    vector
    ))

(defparameter *cl-class-symbols*
  '(standard-object structure-object))

(defparameter *cl-condition-type-symbols*
  '(
    arithmetic-error
    cell-error
    condition
    control-error
    division-by-zero
    end-of-file
    error
    file-error
    floating-point-inexact
    floating-point-invalid-operation
    floating-point-overflow
    floating-point-underflow
    package-error
    parse-error
    print-not-readable
    program-error
    reader-error
    serious-condition
    simple-condition
    simple-error
    simple-type-error
    simple-warning
    storage-condition
    stream-error
    style-warning
    type-error
    unbound-slot
    unbound-variable
    undefined-function
    warning
    ))

(defparameter *cl-all-type-symbols*
  (reduce #'union
          (list *cl-type-symbols* *cl-types-that-are-classes-symbols*
                *cl-system-class-symbols* *cl-class-symbols*
                *cl-condition-type-symbols*)))
