;;;; restarts.lisp

(in-package #:portable-condition-system)

;;; Restart definition

(defvar *restart-clusters* '())

(defstruct restart
  (name (error "NAME required."))
  (function (error "FUNCTION required."))
  (report-function nil)
  (interactive-function nil)
  (test-function (constantly t))
  (associated-conditions '()))

(defmethod print-object ((restart restart) stream)
  (cond (*print-escape*
         (print-unreadable-object (restart stream :type t :identity t)
           (princ (restart-name restart) stream)))
        ((restart-report-function restart)
         (funcall (restart-report-function restart) stream))
        ((restart-name restart)
         (prin1 (restart-name restart) stream))
        (t
         (prin1 restart stream))))

;;; Restart functions

(defun restart-visible-p (restart condition)
  (or (null condition)
      (let ((associated-conditions (restart-associated-conditions restart)))
        (or (null associated-conditions)
            (member condition (restart-associated-conditions restart))))))

(defun compute-restarts (&optional condition)
  (let ((all-restarts (copy-list (apply #'append *restart-clusters*))))
    (remove-if-not (lambda (restart) (restart-visible-p restart condition))
                   all-restarts)))

(defun find-restart (name &optional condition)
  (dolist (restart-cluster *restart-clusters*)
    (dolist (restart restart-cluster)
      (when (and (or (eq restart name) (eq (restart-name restart) name))
                 (restart-visible-p restart condition))
        (return-from find-restart restart)))))

(defgeneric invoke-restart (restart &rest arguments)
  (:method (restart &rest arguments)
    (declare (ignore arguments))
    (error "Wrong thing passed to INVOKE-RESTART: ~S" restart))
  (:method ((restart symbol) &rest arguments)
    (let ((real-restart (or (find-restart restart)
                            (error "Restart ~S is not active." restart))))
      (apply #'invoke-restart real-restart arguments)))
  (:method ((restart restart) &rest arguments)
    (apply (restart-function restart) arguments)))

(defgeneric invoke-restart-interactively (restart)
  (:method (restart)
    (error "Wrong thing passed to INVOKE-RESTART-INTERACTIVELY: ~S" restart))
  (:method ((restart symbol))
    (let ((real-restart (or (find-restart restart)
                            (error "Restart ~S is not active." restart))))
      (invoke-restart-interactively real-restart)))
  (:method ((restart restart))
    (let* ((interactive-function (restart-interactive-function restart))
           (arguments (if interactive-function
                          (funcall interactive-function)
                          '())))
      (apply (restart-function restart) arguments))))

;;; RESTART-BIND

(defmacro restart-bind (bindings &body body)
  (flet ((transform-binding (binding)
           (destructuring-bind (name function . arguments) binding
             `(make-restart :name ',name :function ,function ,@arguments))))
    (let ((cluster (mapcar #'transform-binding bindings)))
      `(let ((*restart-clusters* (cons (list ,@cluster) *restart-clusters*)))
         ,@body))))

;;; WITH-CONDITION-RESTARTS

(defmacro with-condition-restarts (condition restarts &body body)
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
  (typecase report
    (null '())
    (string `(:report-function (lambda (stream) (write-string ,report stream))))
    (t `(:report-function #',report))))

(defun restart-case-make-interactive-subform (interactive)
  (typecase interactive
    (null '())
    (t `(:interactive-function #',interactive))))

(defun restart-case-make-test-subform (test)
  (typecase test
    (null '())
    (t `(:test-function #',test))))

(defun restart-case-pop-keywords-from-clause (clause)
  (let ((things clause) report interactive test)
    (macrolet ((handle-keyword (symbol keyword)
                 `(progn
                    (when ,symbol
                      (error "Duplicate ~S in clause ~S." ,keyword clause))
                    (pop things)
                    (setf ,symbol (pop things)))))
      (loop
        (let ((thing (first things)))
          (cond
            ((eq thing :report) (handle-keyword report :report))
            ((eq thing :interactive) (handle-keyword interactive :interactive))
            ((eq thing :test) (handle-keyword test :test))
            ((keywordp thing) (error "Unknown RESTART-CASE keyword ~S." thing))
            (t (return (values report interactive test things)))))))))

(defun restart-case-parse-clause (clause)
  (destructuring-bind (name lambda-list . rest) clause
    (multiple-value-bind (report interactive test body)
        (restart-case-pop-keywords-from-clause rest)
      (let ((tag (gensym (format nil "RESTART-~S-TAG" name)))
            (keywords `(,@(restart-case-make-report-subform report)
                        ,@(restart-case-make-test-subform test)
                        ,@(restart-case-make-interactive-subform interactive))))
        (list name tag keywords lambda-list body)))))

;;; RESTART-CASE - bindings and cases

(defun restart-case-make-restart-binding (temp-var datum)
  (destructuring-bind (name tag keys lambda-list body) datum
    (declare (ignore lambda-list body))
    (let ((lambda-var (gensym "RESTART-ARGS")))
      `(,name
        (lambda (&rest ,lambda-var) (setq ,temp-var ,lambda-var) (go ,tag))
        ,@keys))))

(defun restart-case-make-restart-case (block-tag temp-var datum)
  (destructuring-bind (name tag keys lambda-list body) datum
    (declare (ignore name keys))
    `(,tag
      (return-from ,block-tag (apply (lambda ,lambda-list ,@body) ,temp-var)))))

;;; RESTART-CASE - associating conditions

(defun restart-case-signaling-form-p (expression env)
  (let ((expansion (macroexpand expression env)))
    (and (consp expansion)
         (member (car expansion) '(signal warn error cerror)))))

(defun restart-case-expand-non-cerror (expansion)
  (destructuring-bind (function datum . args) expansion
    (let* ((type (case function
                   (signal 'simple-condition)
                   (warn 'simple-warning)
                   (error 'simple-error)))
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum ,args ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,condition))))))

(defun restart-case-expand-cerror (expansion)
  (destructuring-bind (function format-control datum . args) expansion
    (let* ((type 'simple-error)
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum ,args ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,format-control ,condition))))))

(defun restart-case-expand-signaling-form (expression env)
  (let ((expansion (macroexpand expression env)))
    (case (car expansion)
      ((signal warn error) (restart-case-expand-non-cerror expansion))
      (cerror (restart-case-expand-cerror expansion)))))

;;; RESTART-CASE - main macro body

(defmacro restart-case (expression &body clauses &environment env)
  (let ((block-tag (gensym "RESTART-CASE-BLOCK"))
        (temp-var (gensym "RESTART-CASE-VAR"))
        (data (mapcar #'restart-case-parse-clause clauses)))
    (flet ((make-restart-binding (datum)
             (restart-case-make-restart-binding temp-var datum))
           (make-restart-case (datum)
             (restart-case-make-restart-case block-tag temp-var datum)))
      `(block ,block-tag
         (let ((,temp-var nil))
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
  (let ((stream (gensym "STREAM")))
    `(restart-case ,(if (= 1 (length forms)) (car forms) `(progn ,@forms))
       (,restart-name ()
         :report (lambda (,stream) (format ,stream ,format-control ,@args))
         (values nil t)))))

;;; System-defined restarts

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
