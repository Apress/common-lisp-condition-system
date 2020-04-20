;;;; integration/integration.lisp

(uiop:define-package #:portable-condition-system.integration
  (:use #:common-lisp+portable-condition-system
        #:trivial-custom-debugger)
  (:export
   ;; Foreign conditions
   #:foreign-condition #:foreign-warning #:foreign-error
   #:foreign-condition-wrapped-condition
   #:host-condition-to-pcs
   ;; Restarts
   #:host-restart
   #:call-with-host-restarts #:with-host-restarts
   #:host-restart-to-pcs #:host-restart-wrapped-restart
   ;; Integration
   #:debugger))

(in-package #:portable-condition-system.integration)

;;; Foreign conditions

(defun foreign-condition-report (condition stream)
  (format stream "Foreign condition ~S was signaled:~%~A"
          (type-of condition) (foreign-condition-wrapped-condition condition)))

(defgeneric host-condition-to-pcs (condition))

(define-condition foreign-condition ()
  ((wrapped-condition :reader foreign-condition-wrapped-condition
                      :initarg :wrapped-condition))
  (:default-initargs
   :wrapped-condition (error "WRAPPED-CONDITION required."))
  (:report foreign-condition-report))

(defmethod host-condition-to-pcs ((condition cl:error))
  (make-condition 'foreign-error :wrapped-condition condition))

(define-condition foreign-warning (foreign-condition warning) ())

(defmethod host-condition-to-pcs ((condition cl:warning))
  (make-condition 'foreign-warning :wrapped-condition condition))

(define-condition foreign-error (foreign-condition error) ())

(defmethod host-condition-to-pcs ((condition cl:condition))
  (make-condition 'foreign-condition :wrapped-condition condition))

;;; Debugger commands

(defun invoke-maybe-foreign-restart (stream condition names)
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
    (format stream "~&;;  :CONTINUE, :C         Invoke a CONTINUE restart.~%")))

(pushnew #'help-continue-hook portable-condition-system::*help-hooks*)
(pushnew #'help-abort-hook portable-condition-system::*help-hooks*)

;;; Host restarts

(defstruct (host-restart (:include restart))
  (wrapped-restart (error "WRAPPED-RESTART required.")))

(defmethod invoke-restart ((restart host-restart) &rest arguments)
  (cl:invoke-restart (host-restart-wrapped-restart restart) arguments))

(defmethod invoke-restart-interactively ((restart host-restart))
  (cl:invoke-restart-interactively (host-restart-wrapped-restart restart)))

(defun host-restart-to-pcs (host-restart)
  (make-host-restart
   :name (cl:restart-name host-restart)
   :function (lambda (&rest args) (apply #'cl:invoke-restart host-restart args))
   :report-function (lambda (stream) (format stream "(*) ~A" host-restart))
   :wrapped-restart host-restart))

(defun compute-host-restarts (host-condition)
  (let* ((restarts (apply #'append portable-condition-system::*restart-clusters*))
         (old-host-restarts (remove-if-not #'host-restart-p restarts))
         (cl-restarts (cl:compute-restarts host-condition)))
    (loop for cl-restart in cl-restarts
          for maybe-restart = (find cl-restart old-host-restarts
                                    :key #'host-restart-wrapped-restart)
          collect (or maybe-restart (host-restart-to-pcs cl-restart)))))

(defun call-with-host-restarts (condition thunk)
  (let* ((host-restarts (compute-host-restarts
                         (when (typep condition 'foreign-condition)
                           (foreign-condition-wrapped-condition condition))))
         (portable-condition-system::*restart-clusters*
           (append portable-condition-system::*restart-clusters*
                   (list host-restarts))))
    (funcall thunk)))

(defmethod compute-restarts :around (&optional condition)
  (call-with-host-restarts condition #'call-next-method))

(defmethod find-restart :around (name &optional condition)
  (call-with-host-restarts condition #'call-next-method))

;;; Integration

(defmethod invoke-debugger ((host-condition cl:condition))
  (let ((condition (host-condition-to-pcs host-condition)))
    (signal condition)
    (with-host-restarts (host-condition)
      (portable-condition-system::standard-debugger condition))))

(defun debugger (condition hook)
  (let ((*debugger-hook* hook))
    (invoke-debugger condition)))
