;;;; src/conditions.lisp

(in-package #:portable-condition-system)

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

;;; DEFINE-CONDITION

(defun make-condition (type &rest args)
  "Instantiates a new condition object of the provided type with the provided
arguments."
  (apply #'make-instance type args))

(defun define-condition-report-method (name report-option)
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
                (define-condition-report-method name report-option))
            ',name)))

;;; COERCE-TO-CONDITION

(defgeneric coerce-to-condition (datum arguments default-type name)
  (:documentation "Attempts to coerce the provided arguments into a condition
object. The DEFAULT-TYPE argument describes the default condition type that
should be created if no condition type can be inferred from DATUM; the NAME
argument is the name of the coercing operator and is used during invalid
coercions to properly report the error."))

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
