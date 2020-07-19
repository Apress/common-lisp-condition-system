;;;; clcs-22-choice-2-excuse.lisp

(defpackage #:clcs-22-choice-2-excuse
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-22-choice-2-excuse)

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

(defun find-choice (name)
  (loop for choice in *choices*
        when (and (funcall (choice-test-function choice))
                  (eq name (choice-name choice)))
          return choice))

(defun invoke-choice (name &rest arguments)
  (let ((choice (find-choice name)))
    (apply (choice-effect-function choice) arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choice helper functions

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-front-door-p ()
  (format t ";; The front door is~:[ not~;~] locked.~%" *front-door-locked-p*)
  (not *front-door-locked-p*))

(defun perform-escape-through-back-door ()
  (format t ";; Escaping through the back door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-back-door-p ()
  (format t ";; The back door is~:[ not~;~] locked.~%" *back-door-locked-p*)
  (not *back-door-locked-p*))

(defvar *excuses*
  '("Kate did not divide her program into sections properly!"
    "I was borrowing Kate's books on mainframe programming!"
    "I had COBOL-related homework and hoped Kate could help me!"))

(defun perform-excuse (excuse)
  (format t ";; Mark excuses himself before leaving:~%;; \"~A\"~%" excuse)
  (setf *mark-safe-p* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding logic

(defun try-to-hide-mark ()
  (cond ((find-choice 'escape)
         (invoke-choice 'escape))
        (t (format t ";; Kate cannot hide Mark!~%")
           (when (find-choice 'excuse)
             (let ((excuse-text (elt *excuses* (random (length *excuses*)))))
               (invoke-choice 'excuse excuse-text))))))

(defun call-with-home-choices (thunk)
  (let ((*choices*
          (list (make-choice
                 :name 'excuse
                 :effect-function #'perform-excuse)
                (make-choice
                 :name 'escape
                 :effect-function #'perform-escape-through-front-door
                 :test-function #'escape-through-front-door-p)
                (make-choice
                 :name 'escape
                 :effect-function #'perform-escape-through-back-door
                 :test-function #'escape-through-back-door-p))))
    (funcall thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defun test-1 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*front-door-locked-p* nil)
           (*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-2 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*front-door-locked-p* nil))
       (parents-come-back)))))

(defun test-3 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil)
           (*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-4 ()
  (call-with-home-choices
   (lambda ()
     (let ((*mark-safe-p* nil))
       (parents-come-back)))))

#|

CL-USER> (clcs-22-choice-2-excuse:test-1)
;; Uh oh - Kate's parents are back!
;; The front door is not locked.
;; The front door is not locked.
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-22-choice-2-excuse:test-2)
;; Uh oh - Kate's parents are back!
;; The front door is not locked.
;; The front door is not locked.
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-22-choice-2-excuse:test-3)
;; Uh oh - Kate's parents are back!
;; The front door is locked.
;; The back door is not locked.
;; The front door is locked.
;; The back door is not locked.
;; Escaping through the back door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-22-choice-2-excuse:test-4)
;; Uh oh - Kate's parents are back!
;; The front door is locked.
;; The back door is locked.
;; Kate cannot hide Mark!
;; Mark excuses himself before leaving:
;; "Kate did not divide her program into sections properly!"
;; Whew... We're safe! For now.
NIL

|#
