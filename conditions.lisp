;;;; conditions.lisp

(in-package #:portable-condition-system)

;;; DEFINE-CONDITION

(defun make-condition-report-method (name report-option)
  "Accepts the name of the condition being defined and the report option
provided to DEFINE-CONDITION, and returns a DEFMETHOD PRINT-OBJECT form meant to
be spliced into the generated DEFINE-CONDITION expansion."
  (let* ((condition (gensym "CONDITION"))
         (stream (gensym "STREAM"))
         (report (second report-option))
         (report-form (if (stringp report)
                          `(write-string ,report ,stream)
                          `(funcall #',report ,condition ,stream))))
    `((defmethod print-object ((,condition ,name) ,stream)
        ,report-form))))

(defmacro define-condition (name (&rest supertypes) direct-slots &rest options)
  "Defines a new condition type via DEFCLASS, handling the :REPORT options via
defining a PRINT-object method on the newly created class."
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (supertypes (or supertypes '(condition))))
    `(progn (defclass ,name ,supertypes ,direct-slots ,@other-options)
            ,@(when report-option
                (make-condition-report-method name report-option))
            ',name)))

(defun make-condition (type &rest args)
  "Instantiates a new condition object of the provided type with the provided
arguments."
  (apply #'make-instance type args))

;;; Class CONDITION

(defclass condition () ()
  (:documentation "The base condition type that is the supertype of all
condition objects."))

(defmethod print-object ((condition condition) stream)
  "Default condition reporting method which prints the condition type."
  (format stream "Condition ~S was signaled." (type-of condition)))

(defmethod print-object :around ((condition condition) stream)
  "Prints of reports a condition to the provided stream. If *PRINT-ESCAPE* is
bound, the condition is print unreadably; otherwise, it is reported by means of
calling the next printing method."
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t))
      (call-next-method)))

;;; Correctable assertions - utilities

(defun store-value-read-evaluated-form ()
  "Queries the user for a single form to be evaluated, then reads and evaluates
that form."
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((place tag) &body forms)
  "Evaluates the provided forms in an environment with a freshly established
STORE-VALUE restart. The arguments accepted by the macro are the place that
should be set by the restart and the TAGBODY tag that control should be
transferred to after setting the value."
  (let ((report-var (gensym "STORE-VALUE-REPORT"))
        (new-value-var (gensym "NEW-VALUE"))
        (form-or-forms (if (= 1 (length forms)) (first forms) `(progn ,@forms))))
    `(flet ((,report-var (stream)
              (format stream "Supply a new value of ~S." ',place)))
       (restart-case ,form-or-forms
         (store-value (,new-value-var)
           :report ,report-var
           :interactive store-value-read-evaluated-form
           (setf ,place ,new-value-var)
           (go ,tag))))))

;;; Case assertions - common

(defun case-failure (datum operator-name complex-type keys)
  "Signals a CASE-FAILURE error using the provided datum, the name of the case
operator, complex type specifier, and the case keys which were not matched."
  `(error 'case-failure :datum ,datum
                        :expected-type '(,complex-type ,@keys)
                        :name ',operator-name
                        :possibilities ',keys))

(defun check-case-no-otherwise-clause (cases macro-name)
  "Signals an error if any other provided cases is a T/OTHERWISE clause."
  (dolist (case cases)
    (when (member (first case) '(t otherwise))
      (error "~S cases are not allowed in ~S." (first case) macro-name))))

(defun case-accumulate-keys (cases)
  "Collects all keys from the provided cases into a single list."
  (loop for case in cases
        for key-or-keys = (first case)
        if (atom key-or-keys)
          collect key-or-keys
        else
          append key-or-keys))

;;; Case assertions - non-correctable

(defmacro ecase (keyform &rest cases)
  "Evaluates the keyform and checks if it matches any of the keys in the
provided cases. Signals an error otherwise."
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "ECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (case ,keyform ,@cases
             (t ,(case-failure variable 'member 'ecase keys))))))

(defmacro etypecase (keyform &rest cases)
  "Evaluates the keyform and checks if it is of any of the types in the
provided cases. Signals an error otherwise."
  (check-case-no-otherwise-clause cases 'etypecase)
  (let ((keys (case-accumulate-cases cases))
        (variable (gensym "ETYPECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (typecase ,keyform ,@cases
                 (t ,(case-failure variable 'or 'etypecase keys))))))

;;; Case assertions - correctable

(defmacro ccase (keyform &rest cases)
  "Evaluates the keyform (which must be a place) and checks if it matches any of
the keys in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-cases cases))
        (block-name (gensym "CCASE-BLOCK"))
        (tag (gensym "CCASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (case ,keyform ,@cases
                  (t (with-store-value-restart (keyform tag)
                       ,(case-failure keyform 'member 'ccase keys)))))))))

(defmacro ctypecase (keyform &rest cases)
  "Evaluates the keyform (which must be a place) and checks if it is of any of
the types in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-cases cases))
        (block-name (gensym "CTYPECASE-BLOCK"))
        (tag (gensym "CTYPECASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (case ,keyform ,@cases
                  (t (with-store-value-restart (keyform tag)
                       ,(case-failure keyform 'or 'ctypecase keys)))))))))

;;; ASSERT

(defun assert-restart-report (names stream)
  "Reports the restart bound by ASSERT, printing the list of places supplied to
ASSERT, if any."
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}." (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  "Queries the programmer whether they would like to modify the value of a given
place. If not, returns the old value; if yes, queries the programmer for an
expression to evaluate and returns its value.
\
If the old place is a symbol naming a variable, it is dynamically bound by
PROGV to its old value, so the programmer can use it in the newly evaluated
expression."
  (cond
    ((y-or-n-p "The old value of ~S is ~S.~%~
		            Do you want to supply a new value? "
               name value)
     (format *query-io* "~&Type a form to be evaluated:~%")
     (eval (read *query-io*))
     (flet ((read-it () (eval (read *query-io*))))
       (cond ((symbolp name)
              (format *query-io*
                      "~&(The old value is bound to the  symbol ~S.)~%" name)
              (progv (list name) (list value) (read-it)))
             (t (read-it)))))
    (t value)))

(defmacro assert (test-form &optional places datum &rest arguments)
  "Evaluates TEST-FORM and checks if it is true; otherwise, signals a
correctable error that allows the programmer to retry the assertion. If any
places are supplied, the CONTINUE restart allows the programmer to set their
values before retrying the assertion. The optional arguments DATUM and
ARGUMENTS, if supplied, are used to report the assertion error."
  (flet ((make-place-setter (place)
           `(setf ,place (assert-prompt ',place ,place))))
    (let ((tag (gensym "ASSERT-TAG"))
          (error-form (if datum
                          `(error ,datum ,@arguments)
                          `(error "The assertion ~S failed." ',test-form))))
      `(tagbody ,tag
          (unless ,test-form
            (restart-case ,error-form
              (continue ()
                :report (lambda (stream) (assert-report ',places stream))
                ,@(mapcar #'make-place-setter places)
                (go ,tag))))))))

;;; CHECK-TYPE

(defun make-check-type-error (place value type-or-type-string)
  "Instantiates an error object suitable to signal within CHECK-TYPE, using the
provided place, value, and expected type or provided type string."
  (let ((format-control (if (stringp type-or-type-string)
                            "The value of ~S is ~S, which is not ~A."
                            "The value of ~S is ~S, which is not of type ~S.")))
    (make-condition
     'simple-error
     :format-control format-control
     :format-arguments (list place value type-or-type-string))))

(defmacro check-type (place type &optional type-string)
  "Evaluates PLACE and checks if its value is of the provided type. Otherwise,
signals a correctable error with an established STORE-VALUE restart that allows
the programmer to provide a new value for the place before the typecheck is
retried. The optional TYPE-STRING argument is used to construct the report for
the signaled error."
  (let ((block-name (gensym "CHECK-TYPE-BLOCK"))
        (tag (gensym "CHECK-TYPE-TAG"))
        (condition-var (gensym "CONDITION"))
        (type-or-type-string (or type-string `',type)))
    `(block ,block-name
       (tagbody ,tag
          (when (typep ,place ',type) (return-from ,block-name nil))
          (let ((,condition-var (make-check-type-error
                                 ',place ,place ,type-or-type-string)))
            (with-store-value-restart (,place ,tag)
              (error ,condition-var)))))))

;;; COERCE-TO-CONDITION

(defgeneric coerce-to-condition (datum arguments default-type name)
  (:documentation "Attempts to coerce the provided arguments into a condition
object, ."))

(defmethod coerce-to-condition ((datum condition) arguments default-type name)
  "Returns the condition object that was passed to the function. If arguments
are non-NIL, signals a continuable error."
  (when arguments
    (cerror "Ignore the additional arguments."
            'simple-type-error
            :datum arguments
            :expected-type 'null
            :format-control
            "You may not supply additional arguments when giving ~S to ~S."
            :format-arguments (list datum name)))
  datum)

(defmethod coerce-to-condition ((datum symbol) arguments default-type name)
  "Calls MAKE-CONDITION on the provided symbol and arguments."
  (apply #'make-condition datum arguments))

(defmethod coerce-to-condition ((datum string) arguments default-type name)
  "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE, using the DATUM string
as its format control and ARGUMENTS as its format arguments."
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition ((datum function) arguments default-type name)
  "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE, using the DATUM
function as its format control and ARGUMENTS as format arguments."
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition (datum arguments default-type name)
  "Signals an error that the provided datum is not coercable to a condition
object."
  (error 'simple-type-error
         :datum datum
         :expected-type '(or condition symbol function string)
         :format-control "~S is not coercable to a condition."
         :format-arguments (list datum)))

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
    (dolist (cluster *handler-clusters*)
      (dolist (handler cluster)
        (when (typep condition (car handler))
          (funcall (cdr handler) condition))))))

(defun warn (datum &rest arguments)
  "Coerces the provided arguments to a warning condition, establishes a
MUFFLE-WARNING restart, and signals the condition. If the condition is not
handled and the MUFFLE-WARNING restart is not invoked, the condition is reported
to the error output stream."
  (check-type datum (or warning (not condition)))
  (let ((condition
          (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (with-simple-restart (muffle-warning "Muffle the warning.")
      (signal condition)
      (format *error-output* "~&;; Warning:~%~A~%" condition))))

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
             `(progn ,@body))))))

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
