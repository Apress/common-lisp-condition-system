;;;; t/ansi-test-data.lisp

(in-package #:portable-condition-system/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (optimize safety))
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

(defparameter *cl-type-symbols*
  '(atom
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
    unsigned-byte))

(defparameter *cl-types-that-are-classes-symbols*
  '(arithmetic-error
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
    warning))

(defparameter *cl-system-class-symbols*
  '(array
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
    vector))

(defparameter *cl-class-symbols*
  '(standard-object structure-object))

(defparameter *cl-condition-type-symbols*
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
    warning))

(defparameter *cl-all-type-symbols*
  (reduce #'union
          (list *cl-type-symbols* *cl-types-that-are-classes-symbols*
                *cl-system-class-symbols* *cl-class-symbols*
                *cl-condition-type-symbols*)))
