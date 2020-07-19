;;;; clcs-18-lets-meet-kate-and-mark.lisp

(defpackage #:clcs-18-lets-meet-kate-and-mark
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-18-lets-meet-kate-and-mark)

(defvar *mark-safe-p* nil)
(defvar *front-door-locked-p* t)
(defvar *back-door-locked-p* t)

(defun parents-come-back ()
  (format t ";; Uh oh - Kate's parents are back!~%")
  (try-to-hide-mark)
  (if *mark-safe-p*
      (format t ";; Whew... We're safe! For now.~%")
      (we do not want to be here)))

(defun try-to-hide-mark ()
  (format t ";; Some magic happens! Mark turns into a pickle.~%")
  (setf *mark-safe-p* t))

(defun test ()
  (parents-come-back))

#|

CL-USER> (clcs-18-lets-meet-kate-and-mark:test)
;; Uh oh - Kate's parents are back!
;; Some magic happens! Mark turns into a pickle.
;; Whew... We're safe! For now.
NIL

|#
