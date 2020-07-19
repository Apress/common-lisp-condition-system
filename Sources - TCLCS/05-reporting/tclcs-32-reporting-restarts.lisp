;;;; clcs-32-reporting-restarts.lisp

(defpackage #:clcs-32-reporting-restarts
  (:use #:cl)
  (:export #:test-1 #:test-2))

(in-package #:clcs-32-reporting-restarts)

(defvar *toplevel-restarts* '())

(defun compute-relevant-restarts (&optional condition)
  (set-difference (compute-restarts condition) *toplevel-restarts*))

(defvar *excuses*
  '("Kate did not divide her program into sections properly!"
    "I was borrowing Kate's books on mainframe programming!"
    "I had COBOL-related homework and hoped Kate could help me!"))

(defun call-with-home-restarts-1 (thunk)
  (let ((*toplevel-restarts* (compute-restarts))
        (excuse (elt *excuses* (random (length *excuses*)))))
    (flet ((report-excuse (stream) (format stream "Use an excuse, ~S." excuse)))
      (restart-case (funcall thunk)
        (escape ()
          :report "Escape through the back door.")
        (escape ()
          :report (lambda (stream)
                    (write-string "Escape through the front door." stream)))
        (excuse ()
          :report report-excuse)))))

(defun call-with-home-restarts-2 (thunk)
  (let ((*toplevel-restarts* (compute-restarts))
        (excuse (elt *excuses* (random (length *excuses*)))))
    (flet ((report-excuse (stream) (format stream "Use an excuse, ~S." excuse)))
      (restart-bind
          ((escape (lambda ())
                   :report-function
                   (lambda (stream)
                     (write-string "Escape through the back door." stream)))
           (escape (lambda ())
                   :report-function
                   (lambda (stream)
                     (write-string "Escape through the front door." stream)))
           (excuse (lambda ())
                   :report-function #'report-excuse))
        (funcall thunk)))))

(defun test-1 ()
  (call-with-home-restarts-1
   (lambda ()
     (let ((*print-escape* nil))
       (format t ";; Available restarts:~%;;~%~{;; ~W~%~}"
               (compute-relevant-restarts))))))

(defun test-2 ()
  (call-with-home-restarts-1
   (lambda ()
     (let ((*print-escape* nil))
       (format t ";; Available restarts:~%;;~%~{;; ~W~%~}"
               (compute-relevant-restarts))))))

#|

CL-USER> (clcs-32-reporting-restarts:test-1)
;; Available restarts:
;;
;; Use an excuse, "I had COBOL-related homework and hoped Kate could help me!".
;; Escape through the front door.
;; Escape through the back door.
NIL

CL-USER> (clcs-32-reporting-restarts:test-2)
;; Available restarts:
;;
;; Use an excuse, "I had COBOL-related homework and hoped Kate could help me!".
;; Escape through the front door.
;; Escape through the back door.
NIL

|#
