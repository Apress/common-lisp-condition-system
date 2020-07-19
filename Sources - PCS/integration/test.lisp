;;;; integration/test.lisp

(uiop:define-package #:portable-condition-system.integration/test
  (:use #:common-lisp+portable-condition-system
        #:trivial-custom-debugger
        #:portable-condition-system.integration)
  (:export #:run))

(in-package #:portable-condition-system.integration/test)

(defparameter *tests*
  '(wrapping.1 wrapping.2 wrapping.3
    host-break.1
    handler-order.1
    restarts.1
    restart-order.1 restart-order.2 restart-order.3
    abort.1 continue.1 muffle-warning.1 store-value.1 use-value.1
    host-restart-report.1
    debugger.abort.1 debugger.q.1 debugger.continue.1 debugger.c.1))

(defun run ()
  (mapc #'funcall *tests*)
  t)

(defun wrapping.1 ()
  (with-debugger (#'debugger)
    (let ((host-condition (cl:make-condition 'cl:condition)))
      (handler-case (cl:invoke-debugger host-condition)
        (condition (condition)
          (assert (eq host-condition
                      (foreign-condition-wrapped-condition condition))))))))

(defun wrapping.2 ()
  (with-debugger (#'debugger)
    (let ((host-condition (cl:make-condition 'cl:warning)))
      (handler-case (cl:invoke-debugger host-condition)
        (warning (condition)
          (assert (eq host-condition
                      (foreign-condition-wrapped-condition condition))))))))

(defun wrapping.3 ()
  (with-debugger (#'debugger)
    (let ((host-condition (cl:make-condition 'cl:error)))
      (handler-case (cl:invoke-debugger host-condition)
        (error (condition)
          (assert (eq host-condition
                      (foreign-condition-wrapped-condition condition))))))))

(defun host-break.1 ()
  (with-debugger (#'debugger)
    (assert (eq 'good (handler-case (cl:break)
                        (condition () 'good))))))

(defun handler-order.1 ()
  (let ((list '()))
    (flet ((make-pusher (value)
             (lambda (c) (declare (ignore c)) (push value list))))
      (with-debugger (#'debugger)
        (handler-case
            (handler-bind ((error (make-pusher :pcs-foo)))
              (cl:handler-bind ((cl:error (make-pusher :host-foo)))
                (handler-bind ((error (make-pusher :pcs-bar)))
                  (cl:handler-bind ((cl:error (make-pusher :host-bar)))
                    (cl:error "Foo")))))
          (error ()))))
    (assert (equal (nreverse list) '(:host-bar :host-foo :pcs-bar :pcs-foo)))))

(defun restarts.1 ()
  (let ((host-restarts (cl:compute-restarts))
        (pcs-restarts (compute-restarts)))
    (loop for host-restart in host-restarts
          for pcs-restart in pcs-restarts
          for host-restart-too = (host-restart-wrapped-restart pcs-restart)
          do (assert (eq host-restart host-restart-too)))))

(defun sorted-set-difference (list-1 list-2)
  (loop for elt in list-1
        unless (member elt list-2)
          collect elt))

(defun restart-order.1 ()
  (let ((list '()))
    (flet ((make-pusher (value)
             (lambda (&rest args)
               (declare (ignore args))
               (push value list))))
      (with-debugger (#'debugger)
        (cl:restart-case
            (restart-bind ((continue (make-pusher :pcs-foo)))
              (cl:restart-bind ((cl:continue (make-pusher :host-foo)))
                (restart-bind ((continue (make-pusher :pcs-bar)))
                  (cl:restart-bind ((cl:continue (make-pusher :host-bar)))
                    (mapc #'invoke-restart
                          (compute-restarts))))))
          (cl:continue (&rest args) (declare (ignore args))))))
    (assert (equal (nreverse list)
                   '(:pcs-bar :pcs-foo :host-bar :host-foo)))))

(defun restart-order.2 ()
  ;; Test that WITH-HOST-RESTARTS preserves restart identity.
  (let ((list '())
        (toplevel-restarts (compute-restarts)))
    (flet ((make-pusher (value)
             (lambda (&rest args)
               (declare (ignore args))
               (push value list))))
      (with-debugger (#'debugger)
        (cl:restart-case
            (restart-bind ((continue (make-pusher :pcs-foo)))
              (cl:restart-bind ((cl:continue (make-pusher :host-foo)))
                (restart-bind ((continue (make-pusher :pcs-bar)))
                  (cl:restart-bind ((cl:continue (make-pusher :host-bar)))
                    (mapc #'invoke-restart
                          (sorted-set-difference
                           (compute-restarts)
                           toplevel-restarts))))))
          (continue (&rest args) (declare (ignore args))))))
    (assert (equal (nreverse list)
                   '(:pcs-bar :pcs-foo :host-bar :host-foo)))))

(defun restart-order.3 ()
  ;; Limitation: host restarts established inside WITH-HOST-RESTARTS are not
  ;; recognized by PCS.
  (let ((list '())
        (toplevel-restarts (compute-restarts)))
    (flet ((make-pusher (value)
             (lambda (&rest args)
               (declare (ignore args))
               (push value list))))
      (with-debugger (#'debugger)
        (cl:restart-case
            (restart-bind ((continue (make-pusher :pcs-foo)))
              (cl:restart-bind ((cl:continue (make-pusher :host-foo)))
                (restart-bind ((continue (make-pusher :pcs-bar)))
                  (cl:restart-bind ((cl:continue (make-pusher :host-bar)))
                    (mapc #'invoke-restart
                          (sorted-set-difference
                           (compute-restarts)
                           toplevel-restarts))))))
          (continue (&rest args) (declare (ignore args))))))
    (assert (equal (nreverse list)
                   '(:pcs-bar :pcs-foo :host-bar :host-foo)))))

(defun abort.1 ()
  (assert (eql 'good (cl:restart-case (abort)
                       (cl:abort () 'good)))))

(defun continue.1 ()
  (assert (eql 'good (cl:restart-case (continue)
                       (cl:continue () 'good)))))

(defun muffle-warning.1 ()
  (assert (eql 'good (cl:restart-case (muffle-warning)
                       (cl:muffle-warning () 'good)))))

(defun store-value.1 ()
  (assert (eql 'good (cl:restart-case (store-value 'good)
                       (cl:store-value (value) value)))))

(defun use-value.1 ()
  (assert (eql 'good (cl:restart-case (use-value 'good)
                       (cl:use-value (value) value)))))

(defun host-restart-report.1 ()
  (assert (string= "(*) Foo"
                   (cl:restart-case (princ-to-string (find-restart 'abort))
                     (cl:abort () :report "Foo")))))

(defun run-debugger-command
    (command input-string &optional condition &rest args)
  (with-input-from-string (input input-string)
    (with-output-to-string (output)
      (let ((stream (make-two-way-stream input output)))
        (apply #'portable-condition-system::run-debugger-command
               command stream condition args)))))

(defun debugger.abort.1 ()
  (assert (eql (cl:restart-case (run-debugger-command :abort "")
                 (cl:abort () 'good))
               'good)))

(defun debugger.q.1 ()
  (assert (eql (cl:restart-case (run-debugger-command :q "")
                 (cl:abort () 'good))
               'good)))

(defun debugger.continue.1 ()
  (assert (eql (cl:restart-case (run-debugger-command :continue "")
                 (cl:continue () 'good))
               'good)))

(defun debugger.c.1 ()
  (assert (eql (cl:restart-case (run-debugger-command :c "")
                 (cl:continue () 'good))
               'good)))
