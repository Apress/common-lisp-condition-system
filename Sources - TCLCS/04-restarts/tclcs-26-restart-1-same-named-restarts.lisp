;;;; clcs-26-restart-1-same-named-restarts.lisp

(defpackage #:clcs-26-restart-1-same-named-restarts
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-26-restart-1-same-named-restarts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relevant restarts

(defvar *toplevel-restarts* '())

(defun compute-relevant-restarts (&optional condition)
  (set-difference (compute-restarts condition) *toplevel-restarts*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic environment

(defvar *mark-safe-p* nil)
(defvar *front-door-locked-p* t)
(defvar *back-door-locked-p* t)

(defun parents-come-back ()
  (format t ";; Uh oh - Kate's parents are back!~%")
  (try-to-hide-mark)
  (if *mark-safe-p*
      (format t ";; Whew... We're safe! For now.~%")
      (we do not want to be here)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restart helper functions

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-front-door-p (condition)
  (declare (ignore condition))
  (not *front-door-locked-p*))

(defun perform-escape-through-back-door ()
  (format t ";; Escaping through the back door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-back-door-p (condition)
  (declare (ignore condition))
  (not *back-door-locked-p*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding logic

(defun try-to-hide-mark ()
  (cond ((find-restart 'escape)
         (invoke-restart 'escape))
        (t (format t ";; Kate cannot hide Mark!~%"))))

(defun call-with-home-restarts (thunk)
  (let ((*toplevel-restarts* (compute-restarts)))
    (restart-bind ((escape
                     #'perform-escape-through-front-door
                     :test-function #'escape-through-front-door-p)
                   (escape
                     #'perform-escape-through-back-door
                     :test-function #'escape-through-back-door-p))
      (funcall thunk))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defun test-1 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*front-door-locked-p* nil)
           (*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-2 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*front-door-locked-p* nil))
       (parents-come-back)))))

(defun test-3 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-4 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*mark-safe-p* nil))
       (try-to-hide-mark)))))

#|

CL-USER> (clcs-26-restart-1-same-named-restarts:test-1)
;; Uh oh - Kate's parents are back!
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-26-restart-1-same-named-restarts:test-2)
;; Uh oh - Kate's parents are back!
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-26-restart-1-same-named-restarts:test-3)
;; Uh oh - Kate's parents are back!
;; Escaping through the back door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-26-restart-1-same-named-restarts:test-4)
;; Kate cannot hide Mark!
NIL

|#
