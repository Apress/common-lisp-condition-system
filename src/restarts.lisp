;;;; src/restarts.lisp

(in-package #:portable-condition-system)

;;; Restart definition

(defvar *restart-clusters* '()
  "A list containing a list of all restart clusters, where a cluster is a list
of restarts established together.")

(defstruct restart
  "A restart structure, implementing the ANSI CL system class RESTART."
  (name (error "NAME required."))
  (function (error "FUNCTION required."))
  (report-function nil)
  (interactive-function nil)
  (test-function (constantly t))
  (associated-conditions '()))

(defmethod print-object :around ((restart restart) stream)
  "Prints of reports a restart to the provided stream. If *PRINT-ESCAPE* is
bound, the restart is printed unreadably; otherwise, it is reported by means of
calling the next printing method."
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (prin1 (restart-name restart) stream))
      (call-next-method)))

(defmethod print-object ((restart restart) stream)
  "Reports a restart object to the provided stream.
\
The restart's report function is called if present; otherwise, the restart's
name name is printed if present; otherwise, the restart object is printed
unreadably."
  (cond ((restart-report-function restart)
         (funcall (restart-report-function restart) stream))
        ((restart-name restart)
         (format stream "Invoke restart ~A." (restart-name restart)))
        (t
         (format stream "Invoke the anonymous restart ~S." restart))))

;;; Restart functions

(defun restart-visible-p (restart condition)
  "Returns true if the restart should be visible when computed for a given
condition object. If the condition is null, return true; otherwise, if the
restart is associated with a different condition, return false; otherwise,
return true."
  (and (funcall (restart-test-function restart) condition)
       (or (null condition)
           (let ((associated-conditions (restart-associated-conditions restart)))
             (or (null associated-conditions)
                 (member condition (restart-associated-conditions restart)))))))

(defgeneric compute-restarts (&optional condition)
  (:documentation "Return a list of all currently established restarts. If the
optional condition argument is supplied, omits all restarts which are associated
with conditions other than the provided one."))

(defmethod compute-restarts (&optional condition)
  "Appends all restart clusters and deletes all restarts which should not be
visible."
  (let ((all-restarts (copy-list (apply #'append *restart-clusters*))))
    (delete-if-not (lambda (restart) (restart-visible-p restart condition))
                   all-restarts)))

(defgeneric find-restart (name &optional condition)
  (:documentation "Finds the first currently established restart with the
provided name. If the optional condition argument is supplied, the search skips
over all restarts which are associated with conditions other than the provided
one."))

(defmethod find-restart (name &optional condition)
  "Walks all restart clusters and returns the first restart with the correct
name and visibility status."
  (dolist (cluster *restart-clusters*)
    (dolist (restart cluster)
      (when (and (or (eq restart name)
                     (eq (restart-name restart) name))
                 (restart-visible-p restart condition))
        (return-from find-restart restart)))))

(defgeneric invoke-restart (restart &rest arguments)
  (:documentation "Invokes a restart with the provided argument. If the restart
is provided by name, this function calls FIND-RESTART with the provided name
first and signals an error if no such restart is available. If the restart
returns normally, returns the value returned by the restart function."))

(defmethod invoke-restart (restart &rest arguments)
  "Signal an error that an invalid argument has been passed to INVOKE-RESTART."
  (declare (ignore arguments))
  (error "Wrong thing passed to INVOKE-RESTART: ~S" restart))

(defmethod invoke-restart ((restart symbol) &rest arguments)
  "Find the restart with the provided name and invoke it."
  (let ((real-restart (or (find-restart restart)
                          (error "Restart ~S is not active." restart))))
    (apply #'invoke-restart real-restart arguments)))

(defmethod invoke-restart ((restart restart) &rest arguments)
  "Apply the restart function to the provided arguments."
  (apply (restart-function restart) arguments))

(defgeneric invoke-restart-interactively (restart)
  (:documentation "Invokes a restart after calling the restart's interactive
function to retrieve a list of arguments for invoking the restart. If the
restart is provided by name, this function calls FIND-RESTART with the provided
name first and signals an error if no such restart is available. If the restart
returns normally, returns the value returned by the restart function."))

(defmethod invoke-restart-interactively (restart)
  "Signal an error that an invalid argument has been passed to
INVOKE-RESTART-INTERACTIVELY."
  (error "Wrong thing passed to INVOKE-RESTART-INTERACTIVELY: ~S" restart))

(defmethod invoke-restart-interactively ((restart symbol))
  "Find the restart with the provided name and invoke it."
  (let ((real-restart (or (find-restart restart)
                          (error "Restart ~S is not active." restart))))
    (invoke-restart-interactively real-restart)))

(defmethod invoke-restart-interactively ((restart restart))
  "Call the restart's interactive function to obtain the list of arguments and
apply the restart functions to them."
  (let* ((interactive-function (restart-interactive-function restart))
         (arguments (if interactive-function
                        (funcall interactive-function)
                        '())))
    (apply (restart-function restart) arguments)))

;;; RESTART-BIND

(defmacro restart-bind (bindings &body body)
  "Executes the body forms in a dynamic context where the newly established
restarts are available."
  (flet ((transform-binding (binding)
           (destructuring-bind (name function . arguments) binding
             `(make-restart :name ',name :function ,function ,@arguments))))
    (let ((cluster (mapcar #'transform-binding bindings)))
      `(let ((*restart-clusters* (cons (list ,@cluster) *restart-clusters*)))
         ,@body))))

;;; WITH-CONDITION-RESTARTS

(defmacro with-condition-restarts (condition restarts &body body)
  "Associates the provided condition object with multiple provided restart
objects within the dynamic scope of the provided body forms."
  (let ((condition-var (gensym "CONDITION"))
        (restarts-var (gensym "RESTARTS"))
        (restart (gensym "RESTART")))
    `(let ((,condition-var ,condition)
           (,restarts-var ,restarts))
       (unwind-protect
            (progn
              (dolist (,restart ,restarts-var)
                (push ,condition-var (restart-associated-conditions ,restart)))
              ,@body)
         (dolist (,restart ,restarts-var)
           (pop (restart-associated-conditions ,restart)))))))

;;; RESTART-CASE - keyword parsing

(defun restart-case-make-report-subform (report)
  "Accepts an argument to HANDLER-CASE's :REPORT option and generates a
:REPORT-FUNCTION subform, meant to be spliced into the HANDLER-BIND binding
generated by HANDLER-CASE."
  (typecase report
    (null '())
    (string `(:report-function (lambda (stream) (write-string ,report stream))))
    (t `(:report-function #',report))))

(defun restart-case-make-interactive-subform (interactive)
  "Accepts an argument to HANDLER-CASE's :INTERACTIVE option and generates a
:INTERACTIVE-FUNCTION subform, meant to be spliced into the HANDLER-BIND binding
generated by HANDLER-CASE."
  (typecase interactive
    (null '())
    (t `(:interactive-function #',interactive))))

(defun restart-case-make-test-subform (test)
  "Accepts an argument to HANDLER-CASE's :TEST option and generates a
:TEST-FUNCTION subform, meant to be spliced into the HANDLER-BIND binding
generated by HANDLER-CASE."
  (typecase test
    (null '())
    (t `(:test-function #',test))))

(defun restart-case-pop-keywords-from-case (case-forms)
  "Accepts the forms of a handler case following the case's lambda list and
parses that form, checking for the presence of :REPORT, :INTERACTIVE, and :TEST
keywords within the case's body. Returns four values: the body forms of the case
remaining after removing the aforementioned keyword-argument pairs, and the
values of :REPORT, :INTERACTIVE, and :TEST keywords found in the body (or NIL,
if the respective keyword was not found)."
  (let ((things case-forms) report interactive test)
    (macrolet ((handle-keyword (symbol keyword)
                 (let ((value (gensym "KEYWORD-VALUE")))
                   `(progn
                      (when ,symbol
                        (error "Duplicate ~S in case ~S." ,keyword case-forms))
                      (pop things)
                      (let ((,value (pop things)))
                        (unless ,value
                          (error "~S may not be NIL in HANDLER-CASE." ,keyword))
                        (setf ,symbol ,value))))))
      (loop
        (let ((thing (first things)))
          (cond
            ((eq thing :report) (handle-keyword report :report))
            ((eq thing :interactive) (handle-keyword interactive :interactive))
            ((eq thing :test) (handle-keyword test :test))
            ((keywordp thing) (error "Unknown RESTART-CASE keyword ~S." thing))
            (t (return (values things report interactive test)))))))))

(defun restart-case-parse-case (case)
  "Parses the provided handler case and returns five values: restart name,
lambda list of the restart case, a list of HANDLER-BIND keywords meant to be
spliced into the handler binding, the body forms of the handler case, and a
globally unique symbol that is meant to be used as a GO tag within the main body
of HANDLER-CASE."
  (destructuring-bind (name lambda-list . rest) case
    (multiple-value-bind (body report interactive test)
        (restart-case-pop-keywords-from-case rest)
      (let ((tag (gensym (format nil "RESTART-~S-TAG" name)))
            (keywords `(,@(restart-case-make-report-subform report)
                        ,@(restart-case-make-test-subform test)
                        ,@(restart-case-make-interactive-subform interactive))))
        (list name lambda-list keywords body tag)))))

;;; RESTART-CASE - bindings and cases

(defun restart-case-make-restart-binding (temp-var parsed-case)
  "Accepts a parsed RESTART-CASE case and generates a RESTART-BIND binding based
on it."
  (destructuring-bind (name lambda-list keys body tag) parsed-case
    (declare (ignore lambda-list body))
    (let ((lambda-var (gensym "RESTART-ARGS")))
      `(,name
        (lambda (&rest ,lambda-var) (setq ,temp-var ,lambda-var) (go ,tag))
        ,@keys))))

(defun restart-case-make-restart-case (block-tag temp-var parsed-case)
  "Accepts a parsed RESTART-CASE case and generates a TAGBODY case based on it."
  (destructuring-bind (name lambda-list keys body tag) parsed-case
    (declare (ignore name keys))
    `(,tag
      (return-from ,block-tag (apply (lambda ,lambda-list ,@body) ,temp-var)))))

;;; RESTART-CASE - associating conditions

(defun restart-case-signaling-form-p (expression env)
  "Returns true if the expression provided to RESTART-CASE is a known
condition-signaling form whose condition should be associated with the restarts
that are newly established by HANDLER-CASE."
  (let ((expansion (macroexpand expression env)))
    (and (consp expansion)
         (member (car expansion) '(signal warn error cerror)))))

(defun restart-case-expand-non-cerror (expansion)
  "Expands the provided SIGNAL, WARN, or ERROR form into a signaling form whose
condition is associated with the restarts that are newly established by
HANDLER-CASE."
  (destructuring-bind (function datum . args) expansion
    (let* ((type (case function
                   (signal 'simple-condition)
                   (warn 'simple-warning)
                   (error 'simple-error)))
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum (list ,@args)
                                              ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,condition))))))

(defun restart-case-expand-cerror (expansion)
  "Expands the provided CERROR form into a signaling form whose condition is
associated with the restarts that are newly established by HANDLER-CASE."
  (destructuring-bind (function format-control datum . args) expansion
    (let* ((type 'simple-error)
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum ',args ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,format-control ,condition))))))

(defun restart-case-expand-signaling-form (expression env)
  "Expands the provided signaling form into a signaling form whose condition is
associated with the restarts that are newly established by HANDLER-CASE."
  (let ((expansion (macroexpand expression env)))
    (case (car expansion)
      ((signal warn error) (restart-case-expand-non-cerror expansion))
      (cerror (restart-case-expand-cerror expansion)))))

;;; RESTART-CASE - main macro body

(defmacro restart-case (expression &body cases &environment env)
  "Executes the body forms in a dynamic context where the newly established
restarts are available. Each restart immediately transfers control to its
restart case upon invocation, executing the body forms of the case and returning
their value from RESTART-CASE."
  (let ((block-tag (gensym "RESTART-CASE-BLOCK"))
        (temp-var (gensym "RESTART-CASE-VAR"))
        (data (mapcar #'restart-case-parse-case cases)))
    (flet ((make-restart-binding (datum)
             (restart-case-make-restart-binding temp-var datum))
           (make-restart-case (datum)
             (restart-case-make-restart-case block-tag temp-var datum)))
      `(block ,block-tag
         (let ((,temp-var nil))
           (declare (ignorable ,temp-var))
           (tagbody
              (restart-bind ,(mapcar #'make-restart-binding data)
                (return-from ,block-tag
                  ,(if (restart-case-signaling-form-p expression env)
                       (restart-case-expand-signaling-form expression env)
                       expression)))
              ,@(apply #'append (mapcar #'make-restart-case data))))))))

;;; WITH-SIMPLE-RESTART

(defmacro with-simple-restart
    ((restart-name format-control &rest args) &body forms)
  "Executes the body forms in a dynamic context where the newly established
restart is available. This restart restart immediately transfers control outside
the form upon invocation, returning NIL as its primary value and T as its
secondary value."
  (let ((stream (gensym "STREAM")))
    `(restart-case ,(if (= 1 (length forms)) (car forms) `(progn ,@forms))
       (,restart-name ()
         :report (lambda (,stream) (format ,stream ,format-control ,@args))
         (values nil t)))))

;;; System-defined restarts

(defun abort (&optional condition)
  "Invokes the most recent ABORT restart. If the optional condition argument is
provided, instead the most recent ABORT restart that is not associated with any
other condition. If no ABORT restart is available, or the restart fails to
transfer control outside of this form, signals an error."
  (let ((restart (find-restart 'abort condition)))
    (if restart
        (invoke-restart restart)
        (error 'restart-not-found :restart-name 'abort))
    (error 'abort-failure)))

(defun continue (&optional condition)
  "Invokes the most recent CONTINUE restart. If the optional condition argument
is provided, instead the most recent CONTINUE restart that is not associated
with any other condition. If no CONTINUE restart is available, returns NIL."
  (let ((restart (find-restart 'continue condition)))
    (when restart (invoke-restart restart))))

(defun muffle-warning (&optional condition)
  "Invokes the most recent MUFFLE-WARNING restart. If the optional condition
argument is provided, instead the most recent MUFFLE-WARNING restart that is not
associated with any other condition. If no MUFFLE-WARNING restart is available,
signals an error."
  (let ((restart (find-restart 'muffle-warning condition)))
    (if restart
        (invoke-restart restart)
        (error 'restart-not-found :restart-name 'muffle-warning))))

(defun store-value (value &optional condition)
  "Invokes the most recent STORE-VALUE restart. If the optional condition
argument is provided, instead the most recent STORE-VALUE restart that is not
associated with any other condition. If no STORE-VALUE restart is available,
returns NIL."
  (let ((restart (find-restart 'store-value condition)))
    (when restart (invoke-restart restart value))))

(defun use-value (value &optional condition)
  "Invokes the most recent USE-VALUE restart. If the optional condition argument
is provided, instead the most recent USE-VALUE restart that is not associated
with any other condition. If no USE-VALUE restart is available, returns NIL."
  (let ((restart (find-restart 'use-value condition)))
    (when restart (invoke-restart restart value))))
