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

(defun case-failure (datum complex-type operator-name keys)
  "Signals a CASE-FAILURE error using the provided datum, the name of the case
operator, complex type specifier, and the case keys which were not matched."
  `(error 'case-failure :datum ,datum
                        :expected-type '(,complex-type ,@keys)
                        :name ',operator-name
                        :possibilities ',keys))

(defun case-transform-t-otherwise-cases (cases)
  "Transforms T/OTHERWISE cases to prevent them from having a special effect
in CASE."
  (loop for case in cases
        for (key . forms) = case
        if (member key '(t otherwise))
          collect (cons (list key) forms)
        else
          collect case))

(defun case-accumulate-keys (cases)
  "Collects all keys from the provided cases into a single list."
  (loop for case in cases
        for key-or-keys = (first case)
        if (listp key-or-keys)
          append key-or-keys
        else
          collect key-or-keys))

;;; Case assertions - non-correctable

(defmacro ecase (keyform &rest cases)
  "Evaluates the keyform and checks if it matches any of the keys in the
provided cases. Signals an error otherwise."
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "ECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (case ,variable ,@(case-transform-t-otherwise-cases cases)
             (t ,(case-failure variable 'member 'ecase keys))))))

(defmacro etypecase (keyform &rest cases)
  "Evaluates the keyform and checks if it is of any of the types in the
provided cases. Signals an error otherwise."
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "ETYPECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (typecase ,keyform ,@cases ;; (case-transform-t-otherwise-cases cases)
                 (t ,(case-failure variable 'or 'etypecase keys))))))

;;; Case assertions - correctable

(defmacro ccase (keyform &rest cases)
  "Evaluates the keyform (which must be a place) and checks if it matches any of
the keys in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
  (let ((keys (case-accumulate-keys cases))
        (block-name (gensym "CCASE-BLOCK"))
        (tag (gensym "CCASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (case ,keyform ,@(case-transform-t-otherwise-cases cases)
                  (t (with-store-value-restart (,keyform ,tag)
                       ,(case-failure keyform 'member 'ccase keys)))))))))

(defmacro ctypecase (keyform &rest cases)
  "Evaluates the keyform (which must be a place) and checks if it is of any of
the types in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
  (let ((keys (case-accumulate-keys cases))
        (block-name (gensym "CTYPECASE-BLOCK"))
        (tag (gensym "CTYPECASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (typecase ,keyform ,@cases ;; (case-transform-t-otherwise-cases cases)
                      (t (with-store-value-restart (,keyform ,tag)
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
                :report (lambda (stream)
                          (assert-restart-report ',places stream))
                ,@(mapcar #'make-place-setter places)
                (go ,tag))))))))

;;; CHECK-TYPE

(defun make-check-type-error (place value type type-string)
  "Instantiates an error object suitable to signal within CHECK-TYPE, using the
provided place, value, and expected type or provided type string."
  (let ((format-control (if type-string
                            "The value of ~S is ~S, which is not ~A."
                            "The value of ~S is ~S, which is not of type ~S.")))
    (make-condition
     'simple-type-error
     :datum value
     :expected-type type
     :format-control format-control
     :format-arguments (list place value (or type-string type)))))

(defmacro check-type (place type &optional type-string)
  "Evaluates PLACE and checks if its value is of the provided type. Otherwise,
signals a correctable error with an established STORE-VALUE restart that allows
the programmer to provide a new value for the place before the typecheck is
retried. The optional TYPE-STRING argument is used to construct the report for
the signaled error."
  (let ((block-name (gensym "CHECK-TYPE-BLOCK"))
        (tag (gensym "CHECK-TYPE-TAG"))
        (condition-var (gensym "CONDITION")))
    `(block ,block-name
       (tagbody ,tag
          (when (typep ,place ',type) (return-from ,block-name nil))
          (let ((,condition-var (make-check-type-error
                                 ',place ,place ',type ,type-string)))
            (with-store-value-restart (,place ,tag)
              (error ,condition-var)))))))

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
