;;;; clcs-19-choice-1-escape-through-front-door.lisp

(defpackage #:clcs-19-choice-1-escape-through-front-door
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-19-choice-1-escape-through-front-door)

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
;;; Choices

(defvar *choices* '())

(defstruct choice
  (name (error "Must provide :NAME."))
  (effect-function (error "Must provide :EFFECT-FUNCTION."))
  (test-function (constantly t)))

(defun compute-choices ()
  (loop for choice in *choices*
        when (funcall (choice-test-function choice))
          collect choice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choice helper functions

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-front-door-p ()
  (format t ";; The front door is~:[ not~;~] locked.~%" *front-door-locked-p*)
  (not *front-door-locked-p*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding logic

(defun try-to-hide-mark ()
  (let ((choices (compute-choices)))
    (if choices
        (let ((choice (first choices)))
          (format t ";; Performing ~A.~%" (choice-name choice))
          (funcall (choice-effect-function choice)))
        (format t ";; Kate cannot hide Mark!~%"))))

(defun call-with-home-choices (thunk)
  (let ((*choices*
          (list (make-choice
                 :name 'escape-through-front-door
                 :effect-function #'perform-escape-through-front-door
                 :test-function #'escape-through-front-door-p))))
    (funcall thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defun test-1 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*front-door-locked-p* nil))
       (parents-come-back)))))

(defun test-2 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil))
       (try-to-hide-mark)))))

#|

CL-USER> (clcs-19-choice-1-escape-through-front-door:test-1)
;; Uh oh - Kate's parents are back!
;; The front door is not locked.
;; Performing ESCAPE-THROUGH-FRONT-DOOR.
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-19-choice-1-escape-through-front-door:test-2)
;; The front door is locked.
;; Kate cannot hide Mark!
NIL

|#
