;;;; clcs-23-lets-meet-kate-and-mark-again.lisp

(defpackage #:clcs-23-lets-meet-kate-and-mark-again
  (:use #:cl)
  (:export #:test))

(in-package #:clcs-23-lets-meet-kate-and-mark-again)

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

CL-USER> (clcs-23-lets-meet-kate-and-mark-again:test)
;; Uh oh - Kate's parents are back!
;; Some magic happens! Mark turns into a pickle.
;; Whew... We're safe! For now.
NIL

|#
