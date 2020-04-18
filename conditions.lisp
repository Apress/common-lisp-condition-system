;;;; conditions.lisp

(in-package #:portable-condition-system)

;;; DEFINE-CONDITION

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-condition-report-method (name report-option)
    (when report-option
      (let* ((condition (gensym "CONDITION"))
             (stream (gensym "STREAM"))
             (report (second report-option))
             (report-form (if (stringp report)
                              `(write-string ,report ,stream)
                              `(funcall #',report ,condition ,stream))))
        `((defmethod print-object ((,condition ,name) ,stream)
            ,report-form))))))

(defmacro define-condition (name (&rest supertypes) direct-slots &rest options)
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (supertypes (or supertypes '(condition))))
    `(progn (defclass ,name ,supertypes ,direct-slots ,@other-options)
            ,@(make-condition-report-method name report-option)
            ',name)))

(defun make-condition (datum &rest args)
  (apply #'make-instance datum args))

;;; Class CONDITION

(defclass condition () ())

(defmethod print-object ((condition condition) stream)
  (format stream "Condition ~S was signaled." (type-of condition)))

(defmethod print-object :around ((condition condition) stream)
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t))
      (call-next-method)))

;;; Continuable assertions - utilities

(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((place tag) &body forms)
  (let ((report-var (gensym "STORE-VALUE-REPORT"))
        (new-value-var (gensym "NEW-VALUE"))
        (form-or-forms (if (= 1 (length forms)) (first forms) `(progn ,@forms))))
    `(flet ((,report-var (stream)
              (format stream "Supply a new value of ~S." ',place)))
       (restart-case ,form-or-forms
         (store-value (,new-value-var)
           :report ,report-var
           :interactive read-evaluated-form
           (setf ,place ,new-value-var)
           (go ,tag))))))

;;; Case assertions - common

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:default-initargs :name (error "NAME required.")
                     :possibilities (error "POSSIBILITIES required."))
  (:report
   (lambda (condition stream)
     (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
             (type-error-datum condition)
             (case-failure-name condition)
             (case-failure-possibilities condition)))))

(defmacro case-failure (variable operator-name keys)
  `(error 'case-failure :datum ,variable
                        :expected-type '(member ,@keys)
                        :name ,operator-name
                        :possibilities ',keys))

(defun check-case-no-otherwise-clause (cases macro-name)
  (dolist (case cases)
    (when (member (first case) '(t otherwise))
      (error "~S cases are not allowed in ~S." (first case) macro-name))))

(defun case-accumulate-cases (cases)
  (loop for case in cases
        for key-or-keys = (first case)
        if (atom key-or-keys)
          collect key-or-keys
        else
          append key-or-keys))

;;; Case assertions - non-continuable

(defmacro ecase (keyform &rest cases)
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-cases cases))
        (variable (gensym "ECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (case ,keyform ,@cases
             (t (case-failure ,variable 'ecase ,keys))))))

(defmacro etypecase (keyform &rest cases)
  (check-case-no-otherwise-clause cases 'etypecase)
  (let ((keys (case-accumulate-cases cases))
        (variable (gensym "ETYPECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (typecase ,keyform ,@cases
                 (t (case-failure ,variable 'etypecase ,keys))))))

;;; Case assertions - continuable

(defmacro ccase (keyform &rest cases)
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-cases cases))
        (block-name (gensym "CCASE-BLOCK"))
        (tag (gensym "CCASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (case ,keyform ,@cases
                  (t (with-store-value-restart (keyform tag)
                       (case-failure ,keyform 'ccase ,keys)))))))))

(defmacro ctypecase (keyform &rest cases)
  (check-case-no-otherwise-clause cases 'ecase)
  (let ((keys (case-accumulate-cases cases))
        (block-name (gensym "CTYPECASE-BLOCK"))
        (tag (gensym "CTYPECASE-TAG")))
    `(block ,block-name
       (tagbody ,tag
          (return-from ,block-name
            (case ,keyform ,@cases
                  (t (with-store-value-restart (keyform tag)
                       (case-failure ,keyform 'ctypecase ,keys)))))))))

;;; ASSERT

(defun assert-restart-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}." (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
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
  (let ((format-control (if (stringp type-or-type-string)
                            "The value of ~S is ~S, which is not ~A."
                            "The value of ~S is ~S, which is not of type ~S.")))
    (make-condition
     'simple-error
     :format-control format-control
     :format-arguments (list place value type-or-type-string))))

(defmacro check-type (place type &optional type-string)
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

(defgeneric coerce-to-condition (datum arguments default-type name))

(defmethod coerce-to-condition ((datum condition) arguments default-type name)
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
  (apply #'make-condition datum arguments))

(defmethod coerce-to-condition ((datum string) arguments default-type name)
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition ((datum function) arguments default-type name)
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition (datum arguments default-type name)
  (error 'simple-type-error
         :datum datum
         :expected-type '(or symbol string)
         :format-control "Bad argument to ~S: ~S"
         :format-arguments (list name datum)))

;;; Condition signaling

(defvar *break-on-signals* nil)

(defvar *handler-clusters* nil)

(defun signal (datum &rest arguments)
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
  (let ((condition
          (coerce-to-condition datum arguments 'simple-warning 'warn)))
    (check-type condition warning "a warning condition")
    (restart-case (signal condition)
      (muffle-warning ()
        :report "Skip warning."
        (return-from warn nil)))
    (format *error-output* "~&Warning:~%~A~%" condition)))

(defun cerror (continue-string datum &rest arguments)
  (with-simple-restart
      (continue "~A"(apply #'format nil continue-string arguments))
    (apply #'error datum arguments))
  nil)

(defun error (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-error 'error)))
    (signal condition)
    (invoke-debugger condition)))

;;; HANDLER-BIND

(defmacro handler-bind (bindings &body forms)
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
  (destructuring-bind (type lambda-list . forms) datum
    (let ((tag (gensym "HANDLER-TAG")))
      (list* tag type lambda-list forms))))

(defun handler-case-make-handler-binding (temp-var datum)
  (destructuring-bind (tag type lambda-list . forms) datum
    (declare (ignore forms))
    (let ((condition (gensym "CONDITION")))
      `(,type (lambda (,condition)
                (declare (ignorable ,condition))
                ,@(when lambda-list `((setf ,temp-var ,condition)))
                (go ,tag))))))

(defun handler-case-make-handler-case (block-name temp-var datum)
  (destructuring-bind (tag type lambda-list . body) datum
    (declare (ignore type))
    `(,tag
      (return-from ,block-name
        ,(if lambda-list
             `(let ((,(first lambda-list) ,temp-var)) ,@body)
             `(progn ,@body))))))

(defun make-handler-case-without-no-error-case (form cases)
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
  (let ((no-error-case-count (count :no-error cases :key #'car)))
    (case no-error-case-count
      (0 (make-handler-case-without-no-error-case form cases))
      (1 (make-handler-case-with-no-error-case form cases))
      (t (error "Multiple :NO-ERROR cases found in HANDLER-CASE.")))))

;;; IGNORE-ERRORS

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
