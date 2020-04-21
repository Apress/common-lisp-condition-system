;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Condition signaling

(defvar *break-on-signals* nil
  "Declares a condition type for which all signaling functions will call BREAK
before actually signaling the condition.")

(defvar *handler-clusters* nil
  "A list containing a list of all handler clusters, where a cluster is a list
of handlers established together.")

(defun signal (datum &rest arguments)
  "Coerces the provided arguments to a condition and signals it, calling all
condition handlers which match the type of the signaled condition."
  (let ((condition
          (coerce-to-condition datum arguments 'simple-condition 'signal)))
    (if (typep condition *break-on-signals*)
        (break "~A~%Break entered because of *BREAK-ON-SIGNALS*."
               condition))
    (loop for (cluster . remaining-clusters) on *handler-clusters*
          do (let ((*handler-clusters* remaining-clusters))
               (dolist (handler cluster)
                 (when (typep condition (car handler))
                   (funcall (cdr handler) condition)))))))

(defun warn (datum &rest arguments)
  "Coerces the provided arguments to a warning condition, establishes a
MUFFLE-WARNING restart, and signals the condition. If the condition is not
handled and the MUFFLE-WARNING restart is not invoked, the condition is reported
to the error output stream."
  (let ((condition
          (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (with-simple-restart (muffle-warning "Muffle the warning.")
      (signal condition)
      (format *error-output* "~&;; Warning:~%~A~%" condition))
    nil))

(defun error (datum &rest arguments)
  "Coerces the provided arguments to an error condition and signals it. If the
condition is not handled, invokes the debugger with that condition."
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-string datum &rest arguments)
  "Coerces the provided arguments to a warning condition, establishes a CONTINUE
restart, and signals the condition. If the condition is not handled, invokes the
debugger with that condition, allowing execution to continue if the CONTINU."
  (with-simple-restart
      (continue "~A"(apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)

;;; HANDLER-BIND

(defmacro handler-bind (bindings &body forms)
  "Executes the body forms in a dynamic context where the newly established
handlers are available."
  (unless (every (lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings: ~S" bindings))
  (flet ((make-binding (binding)
           (destructuring-bind (condition-type function) binding
             `(cons ',condition-type ,function))))
    (let ((cluster (mapcar #'make-binding bindings)))
      `(let ((*handler-clusters* (cons (list ,@cluster) *handler-clusters*)))
         ,@forms))))

;;; HANDLER-CASE - :NO-ERROR present

(defun make-handler-case-with-no-error-case (form cases)
  "Generates the HANDLER-CASE body in situation where a :NO-ERROR case is
provided."
  (let* ((no-error-case (assoc :no-error cases))
         (other-cases (remove no-error-case cases)))
    (let ((normal-return (gensym "NORMAL-RETURN"))
          (error-return  (gensym "ERROR-RETURN")))
      `(block ,error-return
         (multiple-value-call (lambda ,@(cdr no-error-case))
           (block ,normal-return
             (return-from ,error-return
               (handler-case (return-from ,normal-return ,form)
                 ,@other-cases))))))))

;;; HANDLER-CASE - :NO-ERROR not present

(defun handler-case-annotate-case (datum)
  "Annotates a handler case with a unique go tag."
  (destructuring-bind (type lambda-list . forms) datum
    (let ((tag (gensym "HANDLER-TAG")))
      (list* tag type lambda-list forms))))

(defun handler-case-make-handler-binding (temp-var datum)
  "Accepts an annotated HANDLER-CASE case and generates a HANDLER-BIND binding
based on it."
  (destructuring-bind (tag type lambda-list . forms) datum
    (declare (ignore forms))
    (let ((condition (gensym "CONDITION")))
      `(,type (lambda (,condition)
                (declare (ignorable ,condition))
                ,@(when lambda-list `((setf ,temp-var ,condition)))
                (go ,tag))))))

(defun handler-case-make-handler-case (block-name temp-var datum)
  "Accepts an annotated HANDLER-CASE case and generates a TAGBODY case based on
it."
  (destructuring-bind (tag type lambda-list . body) datum
    (declare (ignore type))
    `(,tag
      (return-from ,block-name
        ,(if lambda-list
             `(let ((,(first lambda-list) ,temp-var)) ,@body)
             `(locally ,@body))))))

(defun make-handler-case-without-no-error-case (form cases)
  "Generates the HANDLER-CASE body in situation where no :NO-ERROR case is
provided."
  (let ((block-name (gensym "HANDLER-CASE-BLOCK"))
        (temp-var (gensym "HANDLER-CASE-VAR"))
        (data (mapcar #'handler-case-annotate-case cases)))
    (flet ((make-handler-binding (datum)
             (handler-case-make-handler-binding temp-var datum))
           (make-handler-case (datum)
             (handler-case-make-handler-case block-name temp-var datum)))
      `(block ,block-name
         (let ((,temp-var nil))
           (declare (ignorable ,temp-var))
           (tagbody
              (handler-bind ,(mapcar #'make-handler-binding data)
                (return-from ,block-name ,form))
              ,@(apply #'append (mapcar #'make-handler-case data))))))))

;;; HANDLER-CASE - main macro body

(defmacro handler-case (form &rest cases)
  "Executes the body forms in a dynamic context where the newly established
handlers are available. Each handler immediately transfers control to its
handler case upon invocation, executing the body forms of the case and returning
their value from HANDLER-CASE."
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-case-without-no-error-case form cases))
      (1 (make-handler-case-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE.")))))

;;; IGNORE-ERRORS

(defmacro ignore-errors (&rest forms)
  "Executes the body forms in a dynamic context where a newly established ERROR
handler is available. This handler handler immediately transfers control outside
the form upon invocation, returning NIL as its primary value and the signaled
condition object as its secondary value."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
