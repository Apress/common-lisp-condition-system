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
   #:host-restart-to-pcs #:host-restart-wrapped-restart
   ;; Integration
   #:debugger))

(in-package #:portable-condition-system.integration)

;;; Foreign conditions

(defun foreign-condition-report (condition stream)
  "Prints the type of the foreign condition and then reports it.."
  (format stream "Foreign condition ~S was signaled:~%~A"
          (type-of condition) (foreign-condition-foreign-condition condition)))

(defgeneric host-condition-to-pcs (condition)
  (:documentation "Wraps the host condition object inside a proper
FOREIGN-CONDITION object."))

(define-condition foreign-condition ()
  ((foreign-condition :reader foreign-condition-foreign-condition
                      :initarg :foreign-condition))
  (:documentation "Supertype of all foreign conditions.")
  (:default-initargs
   :foreign-condition (error "FOREIGN-CONDITION required."))
  (:report foreign-condition-report))

(defmethod host-condition-to-pcs ((condition cl:condition))
  "Wraps the foreign condition in a FOREIGN-CONDITION."
  (make-condition 'foreign-condition :foreign-condition condition))

(define-condition foreign-warning (foreign-condition warning) ()
  (:documentation "Supertype of all foreign warning conditions."))

(defmethod host-condition-to-pcs ((condition cl:warning))
  "Wraps the foreign warning in a FOREIGN-WARNING."
  (make-condition 'foreign-warning :foreign-condition condition))

(define-condition foreign-error (foreign-condition error) ()
  (:documentation "Supertype of all foreign error conditions."))

(defmethod host-condition-to-pcs ((condition cl:error))
  "Wraps the foreign error in a FOREIGN-ERROR."
  (make-condition 'foreign-error :foreign-condition condition))

;;; Host restarts

(defstruct (host-restart (:include restart))
  "A structure denoting a restart from the host CL system."
  (wrapped-restart (error "WRAPPED-RESTART required.")))

(defun host-restart-to-pcs (host-restart)
  "Creates a HOST-RESTART structure that wraps the provided host restart."
  (make-host-restart
   :name (cl:restart-name host-restart)
   :function (lambda (&rest args) (apply #'cl:invoke-restart host-restart args))
   :report-function (lambda (stream) (format stream "(*) ~A" host-restart))
   :wrapped-restart host-restart))

(defmethod invoke-restart ((restart host-restart) &rest arguments)
  "Invokes the host restart object using the host's INVOKE-RESTART."
  (apply #'cl:invoke-restart (host-restart-wrapped-restart restart) arguments))

(defmethod invoke-restart-interactively ((restart host-restart))
  "Invokes the host restart object using the host's
INVOKE-RESTART-INTERACTIVELY."
  (cl:invoke-restart-interactively (host-restart-wrapped-restart restart)))

(defun compute-host-restarts (host-condition)
  "Computes a list of all host restarts visible for the provided host condition
object, or all host restarts if the provided argument is NIL. Ensures that the
host restarts' identity is preserved between different invocations of this
function within the same dynamic scope. Returns a list of HOST-RESTART objects."
  (let* ((restarts (apply #'append
                          portable-condition-system::*restart-clusters*))
         (old-host-restarts (remove-if-not #'host-restart-p restarts))
         (cl-restarts (cl:compute-restarts host-condition)))
    (loop for cl-restart in cl-restarts
          for maybe-restart = (find cl-restart old-host-restarts
                                    :key #'host-restart-wrapped-restart)
          collect (or maybe-restart (host-restart-to-pcs cl-restart)))))

(defun call-with-host-restarts (condition thunk)
  "Calls the provided thunk with a list of host restarts available at the bottom
of PCS handler stack. If the provided condition object is a host condition,
the host restarts are computed based on it."
  (let* ((host-restarts (compute-host-restarts
                         (when (typep condition 'foreign-condition)
                           (foreign-condition-foreign-condition condition))))
         (portable-condition-system::*restart-clusters*
           (append portable-condition-system::*restart-clusters*
                   (list host-restarts))))
    (funcall thunk)))

(defmethod compute-restarts :around (&optional condition)
  "Adds the host restarts on the bottom of the handler stack and calls the next
method."
  (call-with-host-restarts condition #'call-next-method))

(defmethod find-restart :around (name &optional condition)
  "If the provided restart name is ABORT, CONTINUE, MUFFLE-WARNING, STORE-VALUE,
or USE-VALUE, searches the host restarts to ensure that the host restarts with
the respective names from the CL package have a chance to be found."
  (if (member name '(abort continue muffle-warning store-value use-value))
      (let ((cl-name (find-symbol (symbol-name name) :cl)))
        (flet ((find-cl-restart () (call-next-method cl-name condition)))
          (or (call-with-host-restarts condition #'call-next-method)
              (call-with-host-restarts condition #'find-cl-restart))))
      (call-with-host-restarts condition #'call-next-method)))

;;; Integration

(defmethod invoke-debugger ((host-condition cl:condition))
  "Wraps the host condition object in a FOREIGN-CONDITION, signals the wrapped
condition in the PCS handler stack, and invokes the PCS debugger."
  (let ((condition (host-condition-to-pcs host-condition)))
    (signal condition)
    (portable-condition-system::standard-debugger condition)))

(defun debugger (condition hook)
  "Standard debugger function, suitable for using in TRIVIAL-CUSTOM-DEBUGGER."
  (let ((*debugger-hook* hook))
    (invoke-debugger condition)))
