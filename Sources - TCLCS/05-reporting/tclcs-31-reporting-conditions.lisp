;;;; clcs-31-reporting-conditions.lisp

(defpackage #:clcs-31-reporting-conditions
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4 #:test-5))

(in-package #:clcs-31-reporting-conditions)

(define-condition person-condition ()
  ((%person :reader person :initarg :person)))

(define-condition before-call (person-condition) ()
  (:report (lambda (condition stream)
             (format stream "We are about to call ~A." (person condition)))))

(define-condition after-call (person-condition) ()
  (:report (lambda (condition stream)
             (format stream "We have just called ~A." (person condition)))))

(define-condition grave-mistake-1 (error)
  ((%reason :reader reason :initarg :reason))
  (:report "We are committing a grave mistake."))

(define-condition grave-mistake-2 (error)
  ((%reason :reader reason :initarg :reason))
  (:report (lambda (condition stream)
             (format stream "We are committing a grave mistake: ~A."
                     (reason condition)))))

(defun report-grave-mistake-3 (condition stream)
  (format stream "We are committing a grave mistake: ~A."
          (reason condition)))

(define-condition grave-mistake-3 (error)
  ((%reason :reader reason :initarg :reason))
  (:report report-grave-mistake-3))

(defun test-1 ()
  (format t ";; ~A~%" (make-condition 'before-call :person :mom)))

(defun test-2 ()
  (format t ";; ~A~%" (make-condition 'after-call :person :mom)))

(defun test-3 ()
  (format t ";; ~A~%" (make-condition 'grave-mistake-1 :reason :about-to-call-your-ex)))

(defun test-4 ()
  (format t ";; ~A~%" (make-condition 'grave-mistake-2 :reason :about-to-call-your-ex)))

(defun test-5 ()
  (format t ";; ~A~%" (make-condition 'grave-mistake-3 :reason :about-to-call-your-ex)))

#|

CL-USER> (clcs-31-reporting-conditions:test-1)
;; We are about to call MOM.
NIL

CL-USER> (clcs-31-reporting-conditions:test-2)
;; We have just called MOM.
NIL

CL-USER> (clcs-31-reporting-conditions:test-3)
;; We are committing a grave mistake.
NIL

CL-USER> (clcs-31-reporting-conditions:test-4)
;; We are committing a grave mistake: ABOUT-TO-CALL-YOUR-EX.
NIL

CL-USER> (clcs-31-reporting-conditions:test-5)
;; We are committing a grave mistake: ABOUT-TO-CALL-YOUR-EX.
NIL

|#
