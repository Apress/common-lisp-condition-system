;;;; integration/integration.lisp

(uiop:define-package #:portable-condition-system.integration
  (:use #:common-lisp+portable-condition-system
        #:trivial-custom-debugger)
  (:export #:debugger
           #:install))

(in-package #:portable-condition-system.integration)

;;; FOREIGN-CONDITION

(defun foreign-condition-report (condition stream)
  (format stream "Foreign condition ~S was signaled:~%~A"
          (type-of condition) (foreign-condition-condition condition)))

(define-condition foreign-condition ()
  ((condition :reader foreign-condition-condition :initarg :condition))
  (:default-initargs :condition (error "CONDITION required."))
  (:report foreign-condition-report))

(define-condition foreign-warning (foreign-condition warning) ())

(define-condition foreign-error (foreign-condition error) ())

(defgeneric cl-condition-to-pcs (condition)
  (:method ((condition cl:error))
    (make-condition 'foreign-error :condition condition))
  (:method ((condition cl:warning))
    (make-condition 'foreign-warning :condition condition))
  (:method ((condition cl:condition))
    (make-condition 'foreign-condition :condition condition)))

;;; Host restarts

(defstruct (cl-restart (:include restart))
  (:wrapped-restart (error "WRAPPED-RESTART required.")))

(defmethod invoke-restart ((restart cl-restart) &rest arguments)
  (cl:invoke-restart (cl-restart-wrapped-restart restart) arguments))

(defmethod invoke-restart-interactively ((restart cl-restart))
  (cl:invoke-restart-interactively (cl-restart-wrapped-restart restart)))

(defun cl-restart-to-pcs (cl-restart)
  (make-cl-restart
   :name (cl:restart-name cl-restart)
   :function (lambda (&rest args) (apply #'cl:invoke-restart cl-restart args))
   :report-function (lambda (stream) (princ cl-restart stream))
   :wrapped-restart cl-restart))

(defun call-with-host-restarts (cl-condition thunk)
  (let* ((cl-restarts (cl:compute-restarts cl-condition))
         (restarts (mapcar #'cl-restart-to-pcs cl-restarts))
         (portable-condition-system::*restart-clusters*
           (append portable-condition-system::*restart-clusters*
                   (list restarts))))
    (funcall thunk)))

(defmacro with-host-restarts ((condition) &body body)
  `(call-with-host-restarts ,condition (lambda () ,@body)))

;;; Debugger commands

(defun invoke-maybe-foreign-restart (stream condition &rest names)
  (let* ((restarts (compute-restarts condition))
         (restart (find-if (lambda (x) (member x names))
                           restarts :key #'restart-name)))
    (if restart
        (invoke-restart-interactively restart)
        (format stream "~&;; There is no active ~A restart.~%"
                (first names)))))

(defmethod portable-condition-system::run-debugger-command :around
    ((command (eql :abort)) stream condition &rest arguments)
  (declare (ignore arguments))
  (invoke-maybe-foreign-restart stream condition '(cl:abort abort)))

(defmethod portable-condition-system::run-debugger-command :around
    ((command (eql :continue)) stream condition &rest arguments)
  (declare (ignore arguments))
  (invoke-maybe-foreign-restart stream condition '(cl:continue continue)))

;;; Debugger help

(defun help-abort-hook (condition stream)
  (when (and (not (find-restart 'abort condition))
             (find-restart 'cl:abort condition))
    (format stream "~&;;  :ABORT, :Q         Invoke an ABORT restart.~%")))

(defun help-continue-hook (condition stream)
  (when (and (not (find-restart 'continue condition))
             (find-restart 'cl:continue condition))
    (format stream "~&;;  :CONTINUE, :Q         Invoke a CONTINUE restart.~%")))

(pushnew #'help-continue-hook portable-condition-system::*help-hooks*)
(pushnew #'help-abort-hook portable-condition-system::*help-hooks*)

;;; Integration

(defun debugger (condition hook)
  (let ((*debugger-hook* hook))
    (invoke-debugger condition)))

(defmethod invoke-debugger ((cl-condition cl:condition))
  (let ((condition (cl-condition-to-pcs cl-condition)))
    (signal condition)
    (with-host-restarts (cl-condition)
      (portable-condition-system::standard-debugger condition))))

(defun install ()
  (install-debugger #'debugger))
