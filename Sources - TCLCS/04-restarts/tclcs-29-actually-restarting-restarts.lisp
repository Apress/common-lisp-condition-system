;;;; clcs-29-actually-restarting-restarts.lisp

(defpackage #:clcs-29-actually-restarting-restarts
  (:use #:cl)
  (:export #:test-1 #:test-2 #:test-3 #:test-4))

(in-package #:clcs-29-actually-restarting-restarts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relevant restarts

(defvar *toplevel-restarts* '())

(defun compute-relevant-restarts (&optional condition)
  (set-difference (compute-restarts condition) *toplevel-restarts*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic environment

(defvar *front-door-locked-p* t)
(defvar *back-door-locked-p* t)

(defun parents-come-back ()
  (format t ";; Uh oh - Kate's parents are back!~%")
  (call-with-home-restarts
   (lambda ()
     (try-to-hide-mark)
     (we do not want to be here)))
  (format t ";; Whew... We're safe! For now.~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restart helper functions

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%"))

(defun escape-through-front-door-p (condition)
  (declare (ignore condition))
  (not *front-door-locked-p*))

(defun perform-escape-through-back-door ()
  (format t ";; Escaping through the back door.~%"))

(defun escape-through-back-door-p (condition)
  (declare (ignore condition))
  (not *back-door-locked-p*))

(defvar *excuses*
  '("Kate did not divide her program into sections properly!"
    "I was borrowing Kate's books on mainframe programming!"
    "I had COBOL-related homework and hoped Kate could help me!"))

(defun perform-excuse (excuse)
  (format t ";; Mark excuses himself before leaving:~%;; \"~A\"~%" excuse))

(defun provide-excuse ()
  (format t ";; Mark is thinking of an excuse...~%")
  (let ((excuse-text (read-line)))
    (list (if (string/= "" excuse-text)
              excuse-text
              (elt *excuses* (random (length *excuses*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding logic

(defun try-to-hide-mark ()
  (cond ((find-restart 'escape)
         (invoke-restart 'escape))
        (t
         (format t ";; Kate cannot hide Mark!~%")
         (when (find-restart 'excuse)
           (invoke-restart-interactively 'excuse)))))

(defun call-with-home-restarts (thunk)
  (let ((*toplevel-restarts* (compute-restarts)))
    (restart-case (funcall thunk)
      (escape () :test escape-through-back-door-p
        (perform-escape-through-back-door))
      (escape () :test escape-through-front-door-p
        (perform-escape-through-front-door))
      (excuse (reason) :interactive provide-excuse
        (perform-excuse reason)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

(defun test-1 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*front-door-locked-p* nil)
           (*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-2 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*front-door-locked-p* nil))
       (parents-come-back)))))

(defun test-3 ()
  (call-with-home-restarts
   (lambda ()
     (let ((*back-door-locked-p* nil))
       (parents-come-back)))))

(defun test-4 ()
  (call-with-home-restarts
   (lambda ()
     (parents-come-back))))

#|

CL-USER> (clcs-29-actually-restarting-restarts:test-1)
;; Uh oh - Kate's parents are back!
;; Escaping through the back door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-29-actually-restarting-restarts:test-2)
;; Uh oh - Kate's parents are back!
;; Escaping through the front door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-29-actually-restarting-restarts:test-3)
;; Uh oh - Kate's parents are back!
;; Escaping through the back door.
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-29-actually-restarting-restarts:test-4)
;; Uh oh - Kate's parents are back!
;; Kate cannot hide Mark!
;; Mark is thinking of an excuse...
I was consulting Kate on which mainframe to buy for my father! ; text input by the user
;; Mark excuses himself before leaving:
;; "I was consulting Kate on which mainframe to buy for my father!"
;; Whew... We're safe! For now.
NIL

CL-USER> (clcs-29-actually-restarting-restarts:test-4)
;; Uh oh - Kate's parents are back!
;; Kate cannot hide Mark!
;; Mark is thinking of an excuse...
                                                               ; empty line provided
;; Mark excuses himself before leaving:
;; "Kate did not divide her program into sections properly!"
;; Whew... We're safe! For now.
NIL

|#
