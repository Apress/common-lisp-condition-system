;;;; restarts-late.lisp

(in-package #:portable-condition-system)

;;; System-defined restarts

(define-condition restart-not-found (control-error)
  (restart-name)
  (:report (lambda (condition stream)
             (format stream "Restart ~S is not active."
                     (restart-not-found-restart-name condition)))))

(define-condition abort-failure (control-error) ()
  (:report "An ABORT restart failed to transfer control."))

(defun abort (&optional condition)
  (let ((restart (find-restart 'abort condition)))
    (if restart
        (invoke-restart restart)
        (error 'restart-not-found :restart-name 'abort))
    (error 'abort-failure)))

(defun continue (&optional condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart (invoke-restart restart))))

(defun muffle-warning (&optional condition)
  (let ((restart (find-restart 'muffle-warning condition)))
    (if restart
        (invoke-restart restart)
        (error 'restart-not-found :restart-name 'muffle-warning))))

(defun store-value (value &optional condition)
  (let ((restart (find-restart 'store-value condition)))
    (when restart (invoke-restart restart value))))

(defun use-value (value &optional condition)
  (let ((restart (find-restart 'use-value condition)))
    (when restart (invoke-restart restart value))))
