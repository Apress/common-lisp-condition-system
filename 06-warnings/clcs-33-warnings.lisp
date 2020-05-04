;;;; clcs-33-warnings.lisp

(defpackage #:clcs-33-warnings
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4 #:test-5 #:test-6))

(in-package #:clcs-33-warnings)

(defun report-grave-warning (condition stream)
  (format stream "We are committing a grave warning: ~A."
          (reason condition)))

(define-condition grave-warning (warning)
  ((%reason :reader reason :initarg :reason))
  (:report report-grave-warning))

(defun test-1 ()
  (warn 'grave-warning :reason :about-to-call-your-ex)
  (handler-case (warn 'grave-warning :reason :about-to-call-your-ex)
    (warning (condition)
      (let ((*print-escape* nil))
        (write-to-string condition)))))

(defun test-2 ()
  (warn (make-condition 'grave-warning :reason :about-to-call-your-ex))
  (handler-case (warn (make-condition 'grave-warning :reason :about-to-call-your-ex))
    (warning (condition)
      (let ((*print-escape* nil))
        (write-to-string condition)))))

(defun test-3 ()
  (warn "Example warning with no arguments.")
  (handler-case (warn "Example warning with no arguments.")
    (warning (condition)
      (let ((*print-escape* nil))
        (list (write-to-string condition) (type-of condition))))))

(defun test-4 ()
  (warn "Example warning with argument ~S." 42)
  (handler-case (warn "Example warning with argument ~S." 42)
    (warning (condition)
      (let ((*print-escape* nil))
        (list (write-to-string condition) (type-of condition))))))

(defun test-5 ()
  (warn "Example warning with argument ~S." 42))

(defun test-6 ()
  (handler-bind ((warning #'muffle-warning))
    (warn "Example warning with argument ~S." 42)))

#|

CL-USER> (clcs-33-warnings:test-1)
WARNING: We are committing a grave warning: ABOUT-TO-CALL-YOUR-EX.
"We are committing a grave warning: ABOUT-TO-CALL-YOUR-EX."

CL-USER> (clcs-33-warnings:test-2)
WARNING: We are committing a grave warning: ABOUT-TO-CALL-YOUR-EX.
"We are committing a grave warning: ABOUT-TO-CALL-YOUR-EX."

CL-USER> (clcs-33-warnings:test-3)
WARNING: Example warning with no arguments.
("Example warning with no arguments." SIMPLE-WARNING)

CL-USER> (clcs-33-warnings:test-4)
WARNING: Example warning with argument 42.
("Example warning with argument 42." SIMPLE-WARNING)

CL-USER> (clcs-33-warnings:test-5)
WARNING: Example warning with argument 42.
NIL

CL-USER> (clcs-33-warnings:test-6)
NIL

|#
